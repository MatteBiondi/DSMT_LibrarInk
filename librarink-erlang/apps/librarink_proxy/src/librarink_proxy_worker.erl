%%%-------------------------------------------------------------------
%%% @doc
%%% Module containing the definition of proxy worker process. The workflow is composed by:
%%% <ol>
%%%   <li>Parse and identify the request</li>
%%%   <li>Forward the request to one or more nodes, selecting the active ones</li>
%%%   <li>Collect the response/responses and join them if necessary</li>
%%%   <li>Forward the JSON encoded response to the original client</li>
%%%   <li>Publish notification on specific queue if necessary</li>
%%% </ol>
%%% @end
%%% Created : 05. set 2022 13:32
%%%-------------------------------------------------------------------
-module(librarink_proxy_worker).
-export([work/4]).

-include_lib("kernel/include/logger.hrl").

-record(librarink_proxy_env, {mnesia_name, mnesia_nodes, request_timeout, mqs_host, mqs_user,
  mqs_password, exchange_type, routing_key}).

-define(CONNECTED_FILTER(Active, Backup), fun(Node) -> (Node =:= Active) or (Node =:= Backup) end).
-define(CONNECTED_NODES, [node()] ++ nodes()).
-define(ADD_NOTIFICATION(Isbn, FirstCopy),{Isbn, jsx:encode(#{isbn => Isbn, operation => add, first_copy => FirstCopy})}).
-define(REMOVE_NOTIFICATION(Isbn),{Isbn, jsx:encode(#{isbn => Isbn, operation => sub})}).
-define(SET_NOTIFICATION(Isbn),{Isbn, jsx:encode(#{isbn => Isbn, operation => reset})}).

%%%%%===================================================================
%%%%% API
%%%%%===================================================================
%% @doc
%% Body of the worker process. The correct format of the request is {Operation, Args} as requested by the
%% {@link librarink_mnesiaDB.} module
%% @end
-spec(work(Request :: term(), From :: pid(), Tag :: reference(), Env :: #librarink_proxy_env{}) -> ok).
work(Request, From, Tag, Env) ->
  %% Check request format and forward
  {Result, Response} =
    case Request of
      {Operation, #{isbn := Isbn_}} when is_atom(Operation) and is_binary(Isbn_) -> forward_request(Request, Env);
      {Operation, Args} when is_atom(Operation) and is_map(Args) -> forward_multi_request(Request, Env);
      _ -> {error, malformed_request}
    end,

  %% Encode response
  EncodedResponse =
    case Response of
      {Counter, Values} when is_list(Values) ->#{counter=>Counter, values=>Values};
      Values when is_list(Values) -> #{values=>Values};
      Value when is_atom(Value) -> Value;
      Value when is_number(Value) -> Value;
      Loan when is_map(Loan) -> Loan;
      Err ->  Err;
      _ -> <<"something went wrong">>
    end,
  try
    %% Send response
    From ! {Tag, jsx:encode(#{result => Result, response => EncodedResponse})},

    %% Send notification, if needed
    {_, RequestArgs} = Request,
    ContainsIsbn = maps:is_key(isbn, RequestArgs),
    if
      ContainsIsbn ->
        {_, #{isbn := Isbn}} = Request,
        case lookup_server(Isbn, Env) of
          {true, Node} ->
            Name = Env#librarink_proxy_env.mnesia_name,
            %% Check number of copies available
            CopiesResponse = gen_server:call(
              {Name, Node},
              {read_copies,  #{type => available, operation => count, isbn => Isbn}},
              Env#librarink_proxy_env.request_timeout
            ),
            case CopiesResponse of
              {succeed, Copies} ->
                {Exchange, Notification} = build_notification(Request, Result, Copies),
                try
                  case Notification of
                    no_notification -> ok;
                    _ -> publish_notification(Exchange, Notification, Env)
                  end
                catch
                  error: Error -> ?LOG_WARNING("Notification publishment failed: ~p",[Error])
                end;
              _ -> ok
            end;
          false -> {error, server_unavailable}
        end;
      true -> ok
    end
  catch
    error: _ ->  From ! {Tag, jsx:encode(#{result => error, response => unexpected_error})};
    exit:{timeout, _} -> From ! {Tag, jsx:encode(#{result => error, response => timeout_exceeded})}
  end.

%%%%%===================================================================
%%%%% INTERNAL FUNCTIONS
%%%%%===================================================================
%% @private
%% @doc
%% Forward the request just to one isbn if the operation involves only one isbn
%% @end
-spec(forward_request(Request :: term(), Env :: #librarink_proxy_env{}) -> {succeed, Response :: term()} | {error,
  Reason :: term()}).
forward_request(Request = {_Operation, #{isbn := Isbn}}, Env) ->
  case lookup_server(Isbn, Env) of
    {true, Node} ->
      Name = Env#librarink_proxy_env.mnesia_name,
      try
        gen_server:call({Name, Node}, Request, Env#librarink_proxy_env.request_timeout)
      catch
        exit:{timeout, _}  -> {error, timeout_exceeded}
      end;
    false -> {error, server_unavailable}
  end.

%% @private
%% @doc
%% Forward a request to multiple nodes if the operation involves more isbn
%% @end
-spec(forward_multi_request(Request :: term(), Env :: #librarink_proxy_env{}) -> {succeed, Response :: term()} | {error,
  Reason :: term()}).
forward_multi_request(Request, Env) ->
  ExpectedNodesSize = length(Env#librarink_proxy_env.mnesia_nodes),
  case lists:filtermap(fun({Active, Backup}) -> retrieve_active_node(Active, Backup, Env) end, Env#librarink_proxy_env
  .mnesia_nodes) of
    Nodes when length(Nodes) =:= ExpectedNodesSize ->
      Name = Env#librarink_proxy_env.mnesia_name,
      try
        case gen_server:multi_call(Nodes, Name, Request, Env#librarink_proxy_env.request_timeout) of
        {Responses, []} -> join_responses(Responses);
        _ -> {error, server_unavailable}
        end
      catch
        exit:{timeout, _}  -> {error, timeout_exceeded}
      end;
    _ -> {error, server_unavailable}
  end.

%% @private
%% @doc
%% Select the correct server that is associated to a certain isbn. The operation of lookup use mapping between the
%% result (GroupId) of a function that takes isbn as argument and node position in the config file
%% @end
-spec(lookup_server(Isbn :: binary(), Env :: #librarink_proxy_env{}) -> Node :: node()).
lookup_server(Isbn,Env) ->
  GroupId = binary:decode_unsigned(crypto:hash(md5, Isbn)) rem erlang:length(Env#librarink_proxy_env.mnesia_nodes) + 1,
  {Active, Backup} = lists:nth(GroupId, Env#librarink_proxy_env.mnesia_nodes),
  retrieve_active_node(Active, Backup, Env).

%% @private
%% @doc
%% Select the node that is currently active
%% @end
-spec(retrieve_active_node(Active :: node(), Backup :: node(), Env :: #librarink_proxy_env{}) -> Node :: node()).
retrieve_active_node(Active, Backup, Env) ->
  ConnectedNodes = length(nodes()),
  if (ConnectedNodes == 0) ->
      Nodes = Env#librarink_proxy_env.mnesia_nodes,
      lists:foreach(fun({ActiveNode, BackupNode}) -> net_adm:ping(ActiveNode),net_adm:ping(BackupNode) end, Nodes);
    true -> ok
  end,
  ResActive = lists:member(Active, ?CONNECTED_NODES),
  ResBackup = lists:member(Backup, ?CONNECTED_NODES),
  if ResActive ->
    {true, Active};
    true ->
      if ResBackup -> {true, Backup};
        true -> false
      end
  end.

%% @private
%% @doc
%% Merge all replies to the multi-call request using a map-reduce approach to handle counters
%% @end
-spec(join_responses(Responses :: list()) -> {succeed, Response :: term()} | {error, Reason :: term()}).
join_responses(Responses) ->
  Map =
    fun({_Node, {Res, Response}}) ->
      if Res =/= succeed -> [{error, Response}]; true -> [Response] end
    end,

  FoldResponses =
    fun({Counter, Items}, {AccCounter, AccList}) -> {AccCounter + Counter, AccList ++ Items} end,

  FoldErrors =
    fun({error, Reason}, Errors) ->
      Duplicate = lists:member(Reason, Errors),
      if Duplicate -> Errors; true -> Errors ++ [Reason] end
    end,

  MappedResponses = lists:flatmap(Map, Responses),
  ErrorList = lists:filter(fun(Elem)-> case Elem of {error, _} -> true; _else -> false end end, MappedResponses),

  case length(ErrorList) of
    0 -> case uniq(MappedResponses) of
           [ok] -> {succeed, ok};
           _ -> {succeed, lists:foldl(FoldResponses, {0, []}, MappedResponses)}
         end;
    _Error -> {errors, lists:foldl(FoldErrors, [], ErrorList)}
  end.

%% @private
%% @doc
%% Publish on MQS queue the notification about a certain event on a specific book
%% @end
-spec(publish_notification(Isbn :: binary(), Notification :: binary(), Env :: #librarink_proxy_env{}) -> none()).
publish_notification(Isbn, Notification, Env) ->
  librarink_common_amqp:produce_once(
    Env#librarink_proxy_env.mqs_host,
    Env#librarink_proxy_env.mqs_user,
    Env#librarink_proxy_env.mqs_password,
    Isbn,
    Env#librarink_proxy_env.exchange_type,
    Env#librarink_proxy_env.routing_key,
    Notification
  ).

%% @private
%% @doc
%% Return the correct notification based on requested operation and relative response
%% @end
-spec(build_notification(Request :: term(), Result :: atom(), Copies :: integer()) -> Notification :: binary()).
build_notification(Request, Result, Copies) ->
  if Result == succeed ->
    case Request of
      {write_copy, #{isbn := Isbn}} when Copies == 1-> ?ADD_NOTIFICATION(Isbn, true);
      {update_reservation, #{isbn := Isbn}} when Copies == 1 -> ?ADD_NOTIFICATION(Isbn, true);
      {update_loan, #{isbn := Isbn}} when Copies == 1 -> ?ADD_NOTIFICATION(Isbn, true);
      {write_copy, #{isbn := Isbn}} when Copies =/= 1-> ?ADD_NOTIFICATION(Isbn, false);
      {update_reservation, #{isbn := Isbn}} when Copies =/= 1 -> ?ADD_NOTIFICATION(Isbn, false);
      {update_loan, #{isbn := Isbn}} when Copies =/= 1 -> ?ADD_NOTIFICATION(Isbn, false);
      {write_reservation, #{isbn := Isbn}} -> ?REMOVE_NOTIFICATION(Isbn);
      {delete_copy, #{isbn := Isbn, id := _}} -> ?SET_NOTIFICATION(Isbn);
      {delete_copy, #{isbn := Isbn}} -> ?REMOVE_NOTIFICATION(Isbn);
      _ -> {"", no_notification}
    end;
    true ->
      {"", no_notification}
  end.


%% @private
%% @doc
%% Utility function to remove duplicates elements from given list (standard lists module introduced this function
%% from Erlang/OTP 25)
%% @end
-spec(uniq(List :: list()) -> UniqList :: list()).
uniq([]) -> [];
uniq([H|T]) -> [H | [X || X <- uniq(T), X /= H]].