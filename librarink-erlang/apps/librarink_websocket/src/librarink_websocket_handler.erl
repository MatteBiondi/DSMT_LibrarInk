%%%-------------------------------------------------------------------
%%% @doc
%%% Websocket handler executed when client open new websocket connection with <a href="https://ninenines.eu">Cowboy</a>
%%% web-server.
%%% @end
%%%-------------------------------------------------------------------
-module(librarink_websocket_handler).

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-include_lib("kernel/include/logger.hrl").

-record(librarink_websocket_state, {mqs_pid = none, items=[]}).

-define(QUEUE_NAME, list_to_binary(pid_to_list(self()))).
-define(EXCHANGE_TYPE, <<"fanout">>).
-define(ROUTING_KEY, <<"">>).
-define(KEEP_ALIVE, <<"keep-alive">>).
-define(GET_ENV(Param),element(2,application:get_env(librarink_websocket, Param))).

%% @doc
%% Entry point of the handler, executed in response to the client websocket connection. The function determines the
%% protocol to use and the relative parameters such as connection timeout and max frame size and then the call {@link
%% websocket_init/1. websocket initialization} function. More on Cowboy <a href="https://ninenines
%% .eu/docs/en/cowboy/2
%% .9/guide/handlers/">documentation</a>.
%% @end
-spec(init(Req, _State) -> {cowboy_websocket, Req, InitState::#librarink_websocket_state{},
  Opts :: #{idle_timeout => TimeOut::integer(), max_frame_size => FrameSize::integer()}}).
init(Req, _State) ->
  Opts = #{idle_timeout => ?GET_ENV(timeout), max_frame_size => ?GET_ENV(framesize)},
  {cowboy_websocket, Req, #librarink_websocket_state{}, Opts}.

%% @doc
%% Websocket handler initialization. Called once at startup initializes the connection with MQS to obtain the updates
%% to forward via websocket to the connected client.
%% @end
-spec(websocket_init(State :: #librarink_websocket_state{}) ->
  {[], #librarink_websocket_state{}, hibernate} |
  {stop, State::#librarink_websocket_state{}}).
websocket_init(State) ->
  ?LOG_NOTICE("Websocket connection established"),
  %% Register to MQS
  case librarink_common_mqs_client:register(self()) of
    {ok, _} -> {[], #librarink_websocket_state{}, hibernate};
    _Error -> {stop, State}
  end.

%% @doc
%% Handle messages received from user via websocket. The message can be:
%% <ol>
%%    <li>Periodic connection keep-alive</li>
%%    <li>JSON encoded list of items to track that defines the bindings of MQS queue</li>
%%    <li>Others, they are ignored</li>
%% </ol>
%% @end
-spec(websocket_handle(IncomingMessage:: term(), State::#librarink_websocket_state{})->
  {ok,  State::#librarink_websocket_state{}, hibernate}).
websocket_handle({text, ?KEEP_ALIVE}, State) ->
 ?LOG_DEBUG("Message on websocket: keep-alive"),
  {ok, State, hibernate};
websocket_handle({text, Msg}, State=#librarink_websocket_state{mqs_pid = MqsPid, items = OldItem}) ->
  ?LOG_DEBUG("Message on websocket: ~p",[Msg]),
  case jsx:is_json(Msg) of
    true ->
      case is_list(NewItem = jsx:decode(Msg, [])) of
        true ->
          add_bindings(MqsPid, ?QUEUE_NAME, lists:subtract(NewItem, OldItem), ?EXCHANGE_TYPE, ?ROUTING_KEY),
          remove_bindings(MqsPid, ?QUEUE_NAME, lists:subtract(OldItem, NewItem), ?ROUTING_KEY),
          ?LOG_DEBUG("New tracked items: ~p", [NewItem]),
          {ok, State#librarink_websocket_state{items = NewItem}, hibernate};
        _false ->
          self() ! {info, <<"Malformed items">>},
          {ok, State, hibernate}
      end;
    _false ->
      self() ! {info, <<"Malformed items">>},
      {ok, State, hibernate}
  end;
websocket_handle(_Msg, State) ->
  ?LOG_NOTICE("Unexpected message: ~p",[_Msg]),
  {ok, State, hibernate}.

%% @doc
%% Handle messages received by process. The messages can be:
%% <ol>
%%  <li>Update sent by MQS process that has received it in the bound queue, these messages will be forwarded to client
%%       via websocket</li>
%%  <li>Info messages generated by the handler itself used to inform client about some unexpected events</li>
%%  <li>Mqs messages, generated by new spawned MQS process to request the configuration about bindings. The message
%%       contains the pid of MQS process because on crashes the pid will be different</li>
%%  <li> Others, they will be ignored </li>
%% </ol>
%% The messages sent to client are encoded in JSON format.
%% @end
-spec(websocket_info(Msg :: term(), State :: #librarink_websocket_state{}) ->
  {ClientMessage :: list(term()), State :: #librarink_websocket_state{}, hibernate} |
  {NoMessage :: none(), State :: #librarink_websocket_state{}, hibernate}).
websocket_info({update, Update}, State) ->
  ?LOG_DEBUG("Websocket info: update message"),
  {[{text, jsx:encode(#{header => update, body => jsx:decode(Update)})}], State, hibernate};
websocket_info({info, Info}, State) ->
  ?LOG_DEBUG("Websocket info: info message~n"),
  {[{text, jsx:encode(#{header => info, body => Info})}], State, hibernate};
websocket_info({mqs, MqsPid}, State=#librarink_websocket_state{items = Items}) when is_pid(MqsPid) ->
  ?LOG_DEBUG("New MQS process: ~p", [MqsPid]),
  librarink_common_mqs_client:declare_queue(MqsPid, ?QUEUE_NAME),
  add_bindings(MqsPid, ?QUEUE_NAME, Items, ?EXCHANGE_TYPE, ?ROUTING_KEY),
  WebSocketHandlerPid = self(),
  librarink_common_mqs_client:start_consumer(MqsPid, fun(Msg) -> WebSocketHandlerPid ! {update, Msg} end),
  NewState = State#librarink_websocket_state{mqs_pid = MqsPid},
  {[], NewState, hibernate};
websocket_info(_Msg, State) ->
  ?LOG_NOTICE("Unexpected message: ~p", [_Msg]),
  {[], State, hibernate}.

%% @doc
%% Websocket handler closing. Called once during closing phase in order to perform some cleaning actions.
%% More on Cowboy <a href="https://ninenines.eu/docs/en/cowboy/2.9/guide/handlers/">documentation</a>.
%% @end
-spec(terminate(Reason :: any(), Request :: any() ,State::#librarink_websocket_state{}) -> ok).
terminate(Reason, _Request, #librarink_websocket_state{mqs_pid = MqsPid}) ->
  ?LOG_NOTICE("Websocket connection closed: ~p",[Reason]),
  librarink_common_mqs_client:unregister(MqsPid),
  ok.

%%===================================================================
%% Internal functions
%%===================================================================

%% @doc
%% @private
%% Add bindings to selected queue, so it's possible to choose dynamically which items need to be tracked
%% @end
add_bindings(_MqsPid, _Queue, [], _ExchangeType, _RoutingKey) -> ok;
add_bindings(MqsPid, QueueName, [ExchangeName | T], ExchangeType, RoutingKey) ->
  librarink_common_mqs_client:bind_queue(MqsPid, QueueName, ExchangeName, ExchangeType, RoutingKey),
  add_bindings(MqsPid, QueueName, T, ExchangeType, RoutingKey).

%% @doc
%% @private
%% Remove bindings to selected queue, so it's possible to choose dynamically which items need to be tracked
%% @end
remove_bindings(_MqsPid, _Queue, [], _RoutingKey) -> ok;
remove_bindings(MqsPid, QueueName, [ExchangeName | T], RoutingKey) ->
  librarink_common_mqs_client:unbind_queue(MqsPid, QueueName, ExchangeName, RoutingKey),
  remove_bindings(MqsPid, QueueName, T, RoutingKey).
