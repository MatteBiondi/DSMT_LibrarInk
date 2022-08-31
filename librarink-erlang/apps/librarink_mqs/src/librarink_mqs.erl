%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. ago 2022 00:27
%%%-------------------------------------------------------------------
-module(librarink_mqs).
-author("Federico").

-behaviour(gen_server).
-include("librarink_mqs_state.hrl").
-include_lib("amqp_client/include/amqp_client.hrl").

%% API
-export([start_link/1, consumer/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server
-spec(start_link(MasterPid :: pid) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MasterPid) ->
  gen_server:start_link(?MODULE, MasterPid, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: pid()) ->
  {ok, State :: #librarink_mqs_state{}} | {ok, State :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(MasterPid) ->
  process_flag(trap_exit, true), %% Process must trap exits to call terminate/2 on supervisor:terminate_child/2
  {ok, Connection, Channel} = start_channel("localhost"), %% TODO: configuration
  MasterPid ! {mqs,self()},
  {ok, ?MQS_STATE(MasterPid, Connection, Channel)}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #librarink_mqs_state{}) ->
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_call(Request, {From,_}, State = #librarink_mqs_state{master_pid = MasterPid}) ->
  io:format("Handle call: ~p~n", [Request]),
  io:format("State: ~p~n", [State]),
  case From == MasterPid of
    true ->
      handle(Request, State);
    _false ->
      {noreply, State}
  end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), State :: #librarink_mqs_state{}) ->
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_cast(_Request, State = #librarink_mqs_state{}) ->
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), State :: #librarink_mqs_state{}) ->
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_info({'EXIT', Consumer, Reason}, State = #librarink_mqs_state{connection = Connection, channel = Channel,
  queues = Queues, consumer = Consumer, callback = Callback}) ->
  io:format("Consumer crashed: ~p~n", [Reason]),
  {ok, NewConsumer, ConsumerTags} = start_consumer({Connection, Channel}, Queues, Callback),
  link(NewConsumer),
  NewState = State#librarink_mqs_state{consumer = NewConsumer, consumer_tags = ConsumerTags},
  io:format("NewState: ~p~n", [NewState]),
  {noreply, NewState};
handle_info(Info, State = #librarink_mqs_state{}) ->
  io:format("Handle info: ~p~n", [Info]),
  {noreply, State}.
%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #librarink_mqs_state{}) -> term()).
terminate(Reason,State = #librarink_mqs_state{connection = Connection, channel = Channel}) ->
  io:format("Terminating: ~p~nState: ~p~n", [Reason, State]),
  close_connection(Connection, Channel),
  ok.


%%===================================================================
%% Internal functions
%%===================================================================


%% @private
%% @doc Handling requests
-spec(handle(Request :: term(), State :: #librarink_mqs_state{}) ->
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle({declare_queue, ExchangeName, ExchangeType, RoutingKey},
    State = #librarink_mqs_state{channel = Channel, queues = Queue}) ->
  {ok, NewQueue} = declare_queue(Channel, ExchangeName, ExchangeType, RoutingKey),
  NewState = State#librarink_mqs_state{queues = Queue ++ NewQueue},
  io:format("NewState: ~p~n", [NewState]),
  {reply, ok, NewState};
handle({start_consumer, Callback}, State = #librarink_mqs_state{connection = Connection, channel = Channel, queues=
  Queues, consumer = Consumer}) ->
  case is_pid(Consumer) of
    true -> {reply, already_started, State};
    _else ->
      {ok, NewConsumer, ConsumerTags} = start_consumer({Connection, Channel}, Queues, Callback),
      link(NewConsumer),
      NewState = State#librarink_mqs_state{consumer = NewConsumer, consumer_tags = ConsumerTags, callback = Callback},
      io:format("NewState: ~p~n", [NewState]),
      {reply, ok, NewState}
  end.

start_channel(Host) ->
  % Connect to broker
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host}),
  % Open channel
  {ok, Channel} =
    amqp_connection:open_channel(Connection),
  {ok, Connection, Channel}.

declare_queue(Channel, ExchangeName, ExchangeType, RoutingKey)->

  % Declare exchange
  amqp_channel:call(
    Channel,
    #'exchange.declare'{exchange = list_to_binary(ExchangeName), type = list_to_binary(ExchangeType)}
  ),

  % Declare queue
  #'queue.declare_ok'{queue = Queue} =
    amqp_channel:call(Channel, #'queue.declare'{exclusive = true}),

  % Bind queue to exchange
  amqp_channel:call(
    Channel,
    #'queue.bind'{exchange = list_to_binary(ExchangeName), routing_key = list_to_binary(RoutingKey), queue = Queue}
  ),
  {ok, [Queue]}.

start_consumer(_Channel, _Consumer, [], Tags) -> Tags;
start_consumer(Channel, Consumer, [Queue|Tail], Tags) ->
  #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}, Consumer),
  start_consumer(Channel, Consumer, Tail, [Tag] ++ Tags).
start_consumer({Connection, Channel}, Queue, Callback) ->
  Consumer = spawn(?MODULE, consumer, [Connection, Channel, Callback]),
  {ok, Consumer, start_consumer(Channel, Consumer, Queue, [])}.

close_connection(Connection, Channel) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection).

consumer(Connection, Channel, Callback) ->
  process_flag(trap_exit, true),
  case is_process_alive(Channel) of
    true ->
      receive
        #'basic.consume_ok'{} ->
          io:format("~p Basic.consume_ok~n", [self()]),
          consumer(Connection, Channel, Callback);
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
          io:format("~p Consumer: ~p~n",[self(), Msg]),
          amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
          Callback(Msg),
          consumer(Connection, Channel, Callback);
        #'basic.cancel_ok'{} ->
          io:format("~p Cancel~n",[self()]),
          cancel;
        Any ->
          io:format("~p Terminated~p~n",[self(),Any])
          %close_connection(Connection, Channel)
      end;
    _False ->
      io:format("Inactive channel~n"),
      close_connection(Connection, Channel)
  end.
%%produce(Channel, ExchangeName, ExchangeType, Payload)->
%%  produce(Channel, ExchangeName, ExchangeType, "", Payload).
%%produce(Channel, ExchangeName, ExchangeType, RoutingKey, Payload)->
%%
%%  %% Declare exchange
%%  amqp_channel:call(
%%    Channel,
%%    #'exchange.declare'{exchange = list_to_binary(ExchangeName), type = list_to_binary(ExchangeType)}
%%  ),
%%
%%  %% Build message
%%  Publish = #'basic.publish'{exchange = list_to_binary(ExchangeName), routing_key = list_to_binary(RoutingKey)},
%%  Props = #'P_basic'{delivery_mode = 2}, %% persistent message
%%  Msg = #amqp_msg{props = Props, payload = list_to_binary(Payload)},
%%
%%  %% Send message
%%  amqp_channel:cast(Channel, Publish, Msg),
%%  ok.