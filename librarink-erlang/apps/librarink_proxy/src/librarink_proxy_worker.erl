%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 05. set 2022 13:32
%%%-------------------------------------------------------------------
-module(librarink_proxy_worker).

-export([work/3]).

-record(librarink_proxy_env, {mnesia_name, mnesia_nodes, request_timeout, mqs_host, exchange_type, routing_key}).

-define(CONNECTED_FILTER(Active, Backup), fun(Node) -> (Node =:= Active) or (Node =:= Backup) end).
-define(CONNECTED_NODES, [node()] ++ nodes()).
-define(ADD_NOTIFICATION(Isbn),{Isbn, jsx:encode(#{<<"isbn">> => list_to_binary(Isbn), <<"add">> => 1})}).
-define(REMOVE_NOTIFICATION(Isbn),{Isbn, jsx:encode(#{<<"isbn">> => list_to_binary(Isbn), <<"sub">> => 1})}).
-define(SET_NOTIFICATION(Isbn),{Isbn, jsx:encode(#{<<"isbn">> => list_to_binary(Isbn), <<"set">> => 0})}).

%%%%%===================================================================
%%%%% API
%%%%%===================================================================

work(Request, Client, Env) ->
  {Result, Response} =
    case Request of
      {Operation, #{isbn := _Isbn}} when is_atom(Operation) and is_list(_Isbn) -> forward_request(Request, Env);
      {Operation, Args} when is_atom(Operation) and is_map(Args) -> forward_multi_request(Request, Env);
      _Others -> {error, malformed_request}
    end,
  gen_server:reply(Client, {Result, Response}),

  {Exchange, Notification} = build_notification(Request, Result),
  case Notification of
    no_notification -> no_notification;
    _ -> publish_notification(Exchange, Notification, Env)
  end.

%%%%%===================================================================
%%%%% INTERNAL FUNCTIONS
%%%%%===================================================================

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

forward_multi_request(Request, Env) ->
  ExpectedNodesSize = length(Env#librarink_proxy_env.mnesia_nodes),
  case lists:filtermap(fun({Active, Backup}) -> retrieve_active_node(Active, Backup) end, Env#librarink_proxy_env.mnesia_nodes) of
    Nodes when length(Nodes) =:= ExpectedNodesSize ->
      io:format("Nodes: ~p~n", [Nodes]),
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

lookup_server(Isbn,Env) ->
  GroupId = binary:decode_unsigned(crypto:hash(md5, Isbn)) rem erlang:length(Env#librarink_proxy_env.mnesia_nodes) + 1,
  {Active, Backup} = lists:nth(GroupId, Env#librarink_proxy_env.mnesia_nodes),
  retrieve_active_node(Active, Backup).

retrieve_active_node(Active, Backup) ->
  ResActive = lists:member(Active, ?CONNECTED_NODES),
  ResBackup = lists:member(Backup, ?CONNECTED_NODES),
  if ResActive ->
    {true, Active};
    true ->
      if ResBackup -> {true, Backup};
        true -> false
      end
  end.

publish_notification(Isbn, Notification, Env) ->
  librarink_mqs_amqp:produce_once(
    Env#librarink_proxy_env.mqs_host,
    list_to_binary(Isbn),
    Env#librarink_proxy_env.exchange_type,
    Env#librarink_proxy_env.routing_key,
    Notification
  ).

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

uniq([]) -> [];
uniq([H|T]) -> [H | [X || X <- uniq(T), X /= H]].

build_notification(Request, Result) ->
  if Result == succeed ->
    case Request of
      {write_copy, #{isbn := Isbn}} -> ?ADD_NOTIFICATION(Isbn);
      {update_reservation, #{isbn := Isbn}} -> ?ADD_NOTIFICATION(Isbn);
      {update_loan, #{isbn := Isbn}} -> ?ADD_NOTIFICATION(Isbn);
      {write_reservation, #{isbn := Isbn}} -> ?REMOVE_NOTIFICATION(Isbn);
      {delete_copy, #{isbn := Isbn, id := _}} -> ?SET_NOTIFICATION(Isbn);
      {delete_copy, #{isbn := Isbn}} -> ?REMOVE_NOTIFICATION(Isbn);
      _ -> {"", no_notification}
    end;
    true ->
      {"", no_notification}
  end.