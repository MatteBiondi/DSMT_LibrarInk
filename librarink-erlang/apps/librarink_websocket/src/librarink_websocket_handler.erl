%%%-------------------------------------------------------------------
%%% @doc
%%% Websocket handler executed when client open new websocket connection with <a href="https://ninenines.eu">Cowboy</a>
%%% web-server.
%%% @end
%%%-------------------------------------------------------------------
-module(librarink_websocket_handler).
-include("include/librarink_websocket_state.hrl").

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

-define(QUEUE_NAME, list_to_binary(pid_to_list(self()))).
-define(EXCHANGE_TYPE, <<"fanout">>).
-define(ROUTING_KEY, <<"">>).
-define(KEEP_ALIVE, <<"keep-alive">>).
%% @doc
%% Entry point of the handler, executed in response to the client websocket connection. The function determines the
%% protocol to use and the relative parameters such as connection timeout and max frame size and then the call {@link
%% websocket_init/1} function. More on Cowboy <a href="https://ninenines.eu/docs/en/cowboy/2
%% .9/guide/handlers/">documentation</a>.
%% @end
-spec(init(Req, _State) -> {cowboy_websocket, Req, InitState::#librarink_websocket_state{},
  Opts :: #{idle_timeout => TimeOut::integer(), max_frame_size => FrameSize::integer()}}).
init(Req, _State) ->
  %% TODO: get opts from state/config file
  TimeOut = 180 * 1000,
  FrameSize = 1024,
  Opts = #{idle_timeout => TimeOut, max_frame_size => FrameSize},
  {cowboy_websocket, Req, #librarink_websocket_state{}, Opts}.

%% @doc
%% Websocket handler initialization. Called once at startup initializes the connection with MQS to obtain the updates
%% to forward via websocket to the connected client.
%% @end
-spec(websocket_init(State :: #librarink_websocket_state{}) ->
  {[], #librarink_websocket_state{}, hibernate} |
  {stop, State::#librarink_websocket_state{}}).
websocket_init(State) ->
  io:format("[~p] Websocket init: ~p~n",[self(), State]),
  %% Register to MQS
  case librarink_mqs_client:register(self()) of
    {ok, _} -> {[], #librarink_websocket_state{}, hibernate};
    _Error -> {stop, State}
  end.

%% @doc
%% Handle messages received from user via websocket. The message can be periodic connection keep-alive or JSON list
%% of items to track via MQS queues. Others messages are ignored.
%% @end
-spec(websocket_handle(IncomingMessage:: term(), State::#librarink_websocket_state{})->
  {ok,  State::#librarink_websocket_state{}, hibernate}).
websocket_handle({text, ?KEEP_ALIVE}, State) ->
  io:format("[~p] Websocket keep-alive~n",[self()]),
  {ok, State, hibernate};
websocket_handle({text, Msg}, State=#librarink_websocket_state{mqs_pid = MqsPid, items = OldItem}) ->
  io:format("[~p] Websocket handle: ~p~n",[self(), Msg]),
  case jsx:is_json(Msg) of
    true ->
      case is_list(NewItem = jsx:decode(Msg, [])) of
        true ->
          add_bindings(MqsPid, ?QUEUE_NAME, lists:subtract(NewItem, OldItem), ?EXCHANGE_TYPE, ?ROUTING_KEY),
          remove_bindings(MqsPid, ?QUEUE_NAME, lists:subtract(OldItem, NewItem), ?ROUTING_KEY),
          io:format("[~p] New items: ~p~n", [self(), NewItem]),
          {ok, State#librarink_websocket_state{items = NewItem}, hibernate};
        _false ->
          self() ! {info, <<"Malformed items">>},
          {ok, State, hibernate}
      end;
    _false ->
      self() ! {info, <<"Malformed items">>},
      {ok, State, hibernate}
  end;
websocket_handle(_, State) ->
  io:format("[~p] Websocket handle: unexpected message~n",[self()]),
  {ok, State, hibernate}.

%% @doc
%% Handle messages received by process. In case of update or info messages forwards them to the client in JSON format.
%% If the message is sent by MQS process means that it requires the initialization to start the consumer.
%% @end
websocket_info({update, Update}, State) ->
  io:format("[~p] Websocket info: update message~n", [self()]),
  {[{text, jsx:encode(#{<<"Update">> => Update})}], State, hibernate};
websocket_info({info, Info}, State) ->
  io:format("[~p] Websocket info: info message~n",[self()]),
  {[{text, jsx:encode(#{<<"Info">> => Info})}], State, hibernate};
websocket_info({mqs, MqsPid}, State=#librarink_websocket_state{items = Items}) when is_pid(MqsPid) ->
  io:format("[~p] New MQS process: ~p~n",[self(), MqsPid]),
  librarink_mqs_client:declare_queue(MqsPid, ?QUEUE_NAME),
  add_bindings(MqsPid, ?QUEUE_NAME, Items, ?EXCHANGE_TYPE, ?ROUTING_KEY),
  WebSocketHandlerPid = self(),
  librarink_mqs_client:start_consumer(MqsPid, fun(Msg) -> WebSocketHandlerPid ! {update, Msg} end),
  NewState = State#librarink_websocket_state{mqs_pid = MqsPid},
  {[], NewState, hibernate};
websocket_info(Msg, State) ->
  io:format("[~p] Websocket info: unexpected message~p~n",[self(),Msg]),
  {[], State, hibernate}.

%% @doc
%% Websocket handler closing. Called once during closing phase in order to perform some cleaning actions.
%% More on Cowboy <a href="https://ninenines.eu/docs/en/cowboy/2.9/guide/handlers/">documentation</a>.
%% @end
-spec(terminate(Reason :: any(), Request :: any() ,State::#librarink_websocket_state{}) -> ok).
terminate(Reason, _Request, #librarink_websocket_state{mqs_pid = MqsPid}) ->
  io:format("[~p] Websocket terminate: ~p~n",[self(), Reason]),
  librarink_mqs_client:unregister(MqsPid),
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
  librarink_mqs_client:bind_queue(MqsPid, QueueName, ExchangeName, ExchangeType, RoutingKey),
  add_bindings(MqsPid, QueueName, T, ExchangeType, RoutingKey).

%% @doc
%% @private
%% Remove bindings to selected queue, so it's possible to choose dynamically which items need to be tracked
%% @end
remove_bindings(_MqsPid, _Queue, [], _RoutingKey) -> ok;
remove_bindings(MqsPid, QueueName, [ExchangeName | T], RoutingKey) ->
  librarink_mqs_client:unbind_queue(MqsPid, QueueName, ExchangeName, RoutingKey),
  remove_bindings(MqsPid, QueueName, T, RoutingKey).
