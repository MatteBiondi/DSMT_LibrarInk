%%%-------------------------------------------------------------------
%%% @doc
%%% Module containing the definition of MQS process, based on OTP/ger_server behaviour. The MQS process is
%%% responsible to interact with message queuing system broker, receive and consume messages from queues. It's
%%% supervised by {@link librarink_mqs_sup. dedicated supervisor}, so can be restored in case of problems.
%%% <img src="images/mqs-schema.svg" style="display: block;width: 100%;margin: 15px;"/>
%%% @end
%%%-------------------------------------------------------------------
-module(librarink_mqs).

-behaviour(gen_server).

%% API
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).  %% gen_server callbacks

-record(librarink_mqs_state, {master_pid, connection = none, channel= none, queues = [],
  consumer = none, consumer_tags = [], callback = none}).

-define(
MQS_STATE(MasterPid, Connection, Channel),
  #librarink_mqs_state{master_pid = MasterPid, connection = Connection, channel = Channel}
).
-define(SERVER, ?MODULE).
-define(GET_ENV(Param),element(2,application:get_env(librarink_mqs, Param))).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc
%% Spawns the server MQS process. The process is not registered because the pid is sent directly to the master
%% process.
%% @end
-spec(start_link(MasterPid :: pid) -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(MasterPid) ->
  gen_server:start_link(?MODULE, MasterPid, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @doc
%% Initializes the server.
%% The initialization includes the request for configuration handled by
%% {@link librarink_websocket_handler:handle_info/2. websocket handler}
%% @end
-spec(init(Args :: pid()) ->
  {ok, State :: #librarink_mqs_state{}} | {ok, State :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(MasterPid) ->
  process_flag(trap_exit, true),  % Process must trap exits to call terminate/2 on supervisor:terminate_child/2 and to
                                  %   receive message in case of consumer process crash
  {ok, Connection, Channel} = librarink_common_amqp:start_connection(?GET_ENV(host)),
  monitor(process, MasterPid), % Start monitor of master process, if it crash MQS process can shutdown
  MasterPid ! {mqs,self()}, % Inform the master process that the MQS process it's ready to receive configuration, in
                            %  case of crash the state will be lost, so the master process has to configure the MQS
                            %  process each time receives this message
  {ok, ?MQS_STATE(MasterPid, Connection, Channel)}.


%% @doc
%% Handle the requests received from master process. The function uses the abstraction provided by
%% {@link librarink_mqs_amqp. librarink_mqs_amqp}. The possible requests are those defined into the {@link
%% librarink_mqs_client. client module}
%% <ol>
%%  <li>Declare queue</li>
%%  <li>Bind queue </li>
%%  <li>Unbind queue</li>
%%  <li>Start consumer</li>
%% </ol>
%% @end
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: #librarink_mqs_state{}) ->
  {reply, Reply :: term(), NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}}).
handle_call(Request, {From,_}, State = #librarink_mqs_state{master_pid = MasterPid}) ->
  io:format("[~p] Handle call: ~p~n", [self(), Request]),
  %io:format("[~p] State: ~p~n", [self(), State]),
  case From == MasterPid of % The request is executed only if sent by master process
    true ->
      handle(Request, State); % Utility function to handler request
    _false ->
      {noreply, State}
  end.

%% @doc
%% Handling cast messages. This semantic is not used for the application.
%% @end
-spec(handle_cast(Request :: term(), State :: #librarink_mqs_state{}) ->
  {noreply, NewState :: #librarink_mqs_state{}} |
  {noreply, NewState :: #librarink_mqs_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_cast(_Request, State) ->
  {noreply, State}.

%% @doc
%% Handling all non call/cast messages. Possible messages are:
%% <ol>
%%  <li>Link: received in case of consumer process crash. The MQS process shutdown gracefully and the supervisor
%%      will spawn a new MQS process.</li>
%%  <li>Monitor: received in case of master process crash. In that case the MQS process request to its supervisor to
%%      stop itself because there is no more need of consuming messages from queues.</li>
%% </ol>
%% All others messages are ignored.
%% @end
-spec(handle_info(Info :: timeout() | term(), State :: #librarink_mqs_state{}) -> % Link message, consumer process crash
  {noreply, NewState :: #librarink_mqs_state{}} | {stop, Reason :: term(), NewState :: #librarink_mqs_state{}}).
handle_info({'EXIT', Consumer, Reason}, State = #librarink_mqs_state{consumer = Consumer}) ->
  io:format("[~p] Consumer crashed: ~p~n", [self(), Reason]),
  {stop, shutdown, State}; % Stop gracefully
handle_info({'DOWN', _Ref, process, MasterPid, Reason},
    State = #librarink_mqs_state{master_pid = MasterPid}) -> % Monitor message, master process crash
  io:format("[~p] Handle monitor: ~p~n", [self(), Reason]),
  spawn(fun() -> librarink_mqs_sup:stop_child(MasterPid ) end), % Request to supervisor to stop the MQS process
  {noreply, State};
handle_info(Info, State = #librarink_mqs_state{}) -> % Catch all clause
  io:format("[~p] Handle info: ~p~n", [self(), Info]),
  {noreply, State}.

%% @private
%% @doc
%% This function is called by a gen_server when it is about to %% terminate. It should be the opposite of
%% Module:init/1 and do any %% necessary cleaning up. When it returns, the gen_server terminates with Reason. The
%% return value is ignored. Before shutting down the connection with MQS broker is closed.
%% @end
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #librarink_mqs_state{}) -> term()).
terminate(Reason,State = #librarink_mqs_state{connection = Connection, channel = Channel}) ->
  io:format("[~p] MQS terminating: ~p~n[~p] State: ~p~n", [self(), Reason, self(), State]),
  librarink_common_amqp:close_connection(Connection, Channel).

%% @private
%% @doc
%% Utility function to handle request from master process using the abstraction layer provided by
%% {@link librarink_mqs_amqp. librarink_mqs_amqp}
%% @end
-spec(handle({Operation :: atom(), Args :: term()}, State :: #librarink_mqs_state{}) ->
  {reply, ok, NewState :: #librarink_mqs_state{}} |
  {reply, undefined_command, NewState :: #librarink_mqs_state{}}).
handle({declare_queue, QueueName}, State = #librarink_mqs_state{channel = Channel, queues = Queues}) ->
  {ok, NewQueue} = librarink_common_amqp:declare_queue(Channel, QueueName),
  NewState = State#librarink_mqs_state{queues = Queues ++ [NewQueue]},
  %io:format("[~p] NewState: ~p~n", [self(), NewState]),
  {reply, ok, NewState};
handle({bind_queue, Queue, ExchangeName, ExchangeType, RoutingKey}, State = #librarink_mqs_state{channel = Channel}) ->
  ok = librarink_common_amqp:bind_queue(Channel, Queue, ExchangeName, ExchangeType, RoutingKey),
  %io:format("[~p] NewState: ~p~n", [self(), State]),
  {reply, ok, State};
handle({unbind_queue, Queue, ExchangeName, RoutingKey}, State = #librarink_mqs_state{channel = Channel}) ->
  librarink_common_amqp:unbind_queue(Channel, Queue, ExchangeName, RoutingKey),
  %io:format("[~p] NewState: ~p~n", [self(), State]),
  {reply, ok, State};
handle({start_consumer, Callback}, State = #librarink_mqs_state{connection = Connection, channel = Channel, queues=
  Queues, consumer = Consumer}) ->
  case is_pid(Consumer) of
    true -> {reply, already_started, State};
    _else ->
      {ok, NewConsumer, ConsumerTags} = librarink_common_amqp:start_consumer({Connection, Channel}, Queues, Callback),
      NewState = State#librarink_mqs_state{consumer = NewConsumer, consumer_tags = ConsumerTags, callback = Callback},
      %io:format("[~p] NewState: ~p~n", [self(), NewState]),
      {reply, ok, NewState}
  end;
handle(Request, State) ->
  io:format("[~p] Undefined command: ~p~n", [self(), Request]),
  {reply, undefined_command, State}.
