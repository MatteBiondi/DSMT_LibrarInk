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

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Spawns the server.
%% @end
-spec(start_link(MasterPid :: pid) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MasterPid) ->
  gen_server:start_link(?MODULE, MasterPid, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc
%% Initializes the server.
%% @end
-spec(init(Args :: pid()) ->
  {ok, State :: #librarink_mqs_state{}} | {ok, State :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(MasterPid) ->
  process_flag(trap_exit, true), %% Process must trap exits to call terminate/2 on supervisor:terminate_child/2
  {ok, Connection, Channel} = librarink_mqs_amqp:start_connection("localhost"), %% TODO: configuration
  monitor(process, MasterPid),
  MasterPid ! {mqs,self()},
  {ok, ?MQS_STATE(MasterPid, Connection, Channel)}.


%% @doc
%% Handling call messages.
%% @end
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #librarink_mqs_state{}) ->
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_call(Request, {From,_}, State = #librarink_mqs_state{master_pid = MasterPid}) ->
  io:format("[~p] Handle call: ~p~n", [self(), Request]),
  io:format("[~p] State: ~p~n", [self(), State]),
  case From == MasterPid of
    true ->
      handle(Request, State);
    _false ->
      {noreply, State}
  end.


%% @doc
%% Handling cast messages. This semantic is not used for this application.
%% @end
-spec(handle_cast(Request :: term(), State :: #librarink_mqs_state{}) ->
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%% @doc
%% Handling all non call/cast messages.
%% @end
-spec(handle_info(Info :: timeout() | term(), State :: #librarink_mqs_state{}) ->
  {noreply, NewState :: #librarink_mqs_state{}} | {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_info({'EXIT', Consumer, Reason}, State = #librarink_mqs_state{consumer = Consumer}) ->
  io:format("[~p] Consumer crashed: ~p~n", [self(), Reason]),
  {stop, shutdown, State};
handle_info({'DOWN', _Ref, process, MasterPid, Reason}, State = #librarink_mqs_state{master_pid = MasterPid}) ->
  io:format("[~p] Handle monitor: ~p~n", [self(), Reason]),
  spawn(fun() -> librarink_mqs_sup:stop_child(MasterPid )end),
  {noreply, State};
handle_info(Info, State = #librarink_mqs_state{}) ->
  io:format("[~p] Handle info: ~p~n", [self(), Info]),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to %% terminate. It should be the opposite of
%% Module:init/1 and do any %% necessary cleaning up. When it returns, the gen_server terminates with Reason. The
%% return value is ignored. Before shutting down the connection with MQS broker is closed.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #librarink_mqs_state{}) -> term()).
terminate(Reason,State = #librarink_mqs_state{connection = Connection, channel = Channel}) ->
  io:format("[~p] MQS terminating: ~p~n[~p] State: ~p~n", [self(), Reason, self(), State]),
  librarink_mqs_amqp:close_connection(Connection, Channel).


%%===================================================================
%% Internal functions
%%===================================================================

%% @doc
%% Handle the requests received from client process
%% @end
-spec(handle(Request :: term(), State :: #librarink_mqs_state{}) ->
  {reply, ok, NewState :: #librarink_mqs_state{}} |
  {reply, undefined_command, NewState :: #librarink_mqs_state{}}).
handle({declare_queue, QueueName}, State = #librarink_mqs_state{channel = Channel, queues = Queues}) ->
  {ok, NewQueue} = librarink_mqs_amqp:declare_queue(Channel, QueueName),
  NewState = State#librarink_mqs_state{queues = Queues ++ NewQueue},
  io:format("[~p] NewState: ~p~n", [self(), NewState]),
  {reply, ok, NewState};
handle({bind_queue, Queue, ExchangeName, ExchangeType, RoutingKey}, State = #librarink_mqs_state{channel = Channel}) ->
  ok = librarink_mqs_amqp:bind_queue(Channel, Queue, ExchangeName, ExchangeType, RoutingKey),
  io:format("[~p] NewState: ~p~n", [self(), State]),
  {reply, ok, State};
handle({unbind_queue, Queue, ExchangeName, RoutingKey}, State = #librarink_mqs_state{channel = Channel}) ->
  librarink_mqs_amqp:unbind_queue(Channel, Queue, ExchangeName, RoutingKey),
  io:format("[~p] NewState: ~p~n", [self(), State]),
  {reply, ok, State};
handle({start_consumer, Callback}, State = #librarink_mqs_state{connection = Connection, channel = Channel, queues=
  Queues, consumer = Consumer}) ->
  case is_pid(Consumer) of
    true -> {reply, already_started, State};
    _else ->
      {ok, NewConsumer, ConsumerTags} = librarink_mqs_amqp:start_consumer({Connection, Channel}, Queues, Callback),
      NewState = State#librarink_mqs_state{consumer = NewConsumer, consumer_tags = ConsumerTags, callback = Callback},
      io:format("[~p] NewState: ~p~n", [self(), NewState]),
      {reply, ok, NewState}
  end;
handle(Request, State) ->
  io:format("[~p] Undefined command: ~p~n", [self(), Request]),
  {reply, undefined_command, State}.
