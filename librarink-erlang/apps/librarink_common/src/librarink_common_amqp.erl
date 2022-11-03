%%%-------------------------------------------------------------------
%%% @doc
%%% Wrapper module that gives a further abstraction of amqp protocol, implemented by the library
%%% <a href="https://github.com/rabbitmq/rabbitmq-server/tree/main/deps/amqp_client" target="_blank">amqp_client</a>
%%% distributed by RabbitMQ. For further information about amqp protocol follows this
%%% <a href="https://www.rabbitmq.com/protocol.html" target="_blank">link</a>.
%%% @end
%%% Created : 01. set 2022 17:49
%%%-------------------------------------------------------------------
-module(librarink_common_amqp).

%% API
-export([start_connection/3, declare_queue/2, bind_queue/5, unbind_queue/4, start_consumer/3, consumer/3,
  close_connection/2, produce/5, produce_once/7]).

-include_lib("amqp_client/include/amqp_client.hrl").
-include_lib("kernel/include/logger.hrl").

%% @doc
%% Connect to the MQS broker at the specified address and open a channel to start communication.
%% @end
-spec(start_connection(Host :: string(),User :: binary(),User :: binary()) -> {ok, Connection :: pid(), Channel :: pid
()}).
start_connection(Host, User, Password) ->
  % Connect to broker
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host, username = User, password = Password}),
  % Open channel
  {ok, Channel} =
    amqp_connection:open_channel(Connection),
  {ok, Connection, Channel}.

%% @doc
%% Declare a queue on specified opened channel.
%% @end
-spec(declare_queue(Channel :: pid(), QueueName :: binary()) -> {ok, Queue :: binary()}).
declare_queue(Channel, QueueName)->

  % Declare queue
  #'queue.declare_ok'{queue = Queue} =
    amqp_channel:call(Channel, #'queue.declare'{queue=QueueName, exclusive = true}),
  {ok, Queue}.

%% @doc
%% Bind selected queue on specified channel with a new exchange defined by the parameters passed as arguments.
%% @end
-spec(bind_queue(Channel :: pid(), Queue :: binary(), ExchangeName :: binary(), ExchangeType :: binary(), RoutingKey
:: binary()) -> ok).
bind_queue(Channel, Queue, ExchangeName, ExchangeType, RoutingKey) ->

  % Declare exchange
  #'exchange.declare_ok'{} = amqp_channel:call(
    Channel,
    #'exchange.declare'{
      exchange = ExchangeName,
      type = ExchangeType
    }),

  % Bind queue to exchange
  #'queue.bind_ok'{} = amqp_channel:call(
    Channel,
    #'queue.bind'{
      exchange = ExchangeName,
      routing_key = RoutingKey,
      queue = Queue
    }),
  ok.

%% @private
%% @doc
%% Support function to handle the subscription of consumer with all queues passed as argument.
%% @end
start_consumer(_Channel, _Consumer, [], Tags) -> Tags;
start_consumer(Channel, Consumer, [Queue|Tail], Tags) ->
  #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}, Consumer),
  start_consumer(Channel, Consumer, Tail, [Tag] ++ Tags).

%% @doc
%% Spawn new process dedicated to consume messages received on registered queues. The consume action is defined by
%% the callback function passed as argument.
%% @end
-spec(start_consumer({Connection :: pid(), Channel :: pid()}, Queues :: list(), Callback :: mfa()) ->
  {ok, Consumer :: pid(), Tags :: list()}).
start_consumer({Connection, Channel}, Queues, Callback) ->
  Consumer = spawn_link(?MODULE, consumer, [Connection, Channel, Callback]),
  {ok, Consumer, start_consumer(Channel, Consumer, Queues, [])}.

%% @doc
%% Remove from selected queue the binding with the exchange passed as arguments.
%% @end
-spec(unbind_queue(Channel :: pid(), Queue :: binary(),ExchangeName :: binary(), RoutingKey :: binary()) ->
  Ret :: #'queue.unbind_ok'{}).
unbind_queue(Channel, Queue, ExchangeName, RoutingKey) when is_binary(Queue), is_binary(ExchangeName), is_binary(RoutingKey) ->
  #'queue.unbind_ok'{}= amqp_channel:call(Channel, #'queue.unbind'{
    queue = Queue,
    exchange = ExchangeName,
    routing_key = RoutingKey
  }),
  ok.

%% @doc
%% Close connection with MQS broker.
%% @end
-spec(close_connection(Connection :: pid(), Channel :: pid()) -> none()).
close_connection(Connection, Channel) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection).

%% @doc
%% Consumer process body. The messages received on queues, after sending the ack, are processed with callback.
%% @end
-spec(consumer(Connection :: pid(), Channel :: pid(), Callback :: mfa()) -> none()).
consumer(Connection, Channel, Callback) ->
  process_flag(trap_exit, true), %% Trap exit in order to close connection in case of MQS process crash
  case is_process_alive(Channel) of
    true ->
      receive
        #'basic.consume_ok'{} -> %% Message received after consumer subscription
          consumer(Connection, Channel, Callback);
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} -> %% Message received on queues
          ?LOG_DEBUG("Consumer message on queue: ~p",[Msg]),
          amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}), %% Send ack to broker
          Callback(Msg),
          consumer(Connection, Channel, Callback);
        #'basic.cancel_ok'{} -> %% Message received after consumer cancellation
          ?LOG_DEBUG("Consumer canceled"),
          cancel;
        {'EXIT',_Pid, Reason}  -> %% Message received if the MQS process that has spawned the consumer crashed
          ?LOG_DEBUG("MQS process crashed: ~p",[Reason]),
          close_connection(Connection, Channel);
        Any -> %% Catch all clause
          ?LOG_NOTICE("Unexpected message: ~p",[Any])
      end;
    _False ->
      ?LOG_NOTICE("Inactive channel"),
      close_connection(Connection, Channel)
  end.

%% @doc
%% Produce message on specified queue.
%% @end
-spec(produce(Channel ::pid(), ExchangeName :: binary(), ExchangeType :: binary(), RoutingKey :: binary(),
    Payload :: binary()) -> none()).
produce(Channel, ExchangeName, ExchangeType, RoutingKey, Payload)->

  %% Declare exchange
  amqp_channel:call(
    Channel,
    #'exchange.declare'{exchange = ExchangeName, type = ExchangeType}
  ),

  %% Build message
  Publish = #'basic.publish'{exchange = ExchangeName, routing_key = RoutingKey},
  Props = #'P_basic'{delivery_mode = 2}, %% persistent message
  Msg = #amqp_msg{props = Props, payload = Payload},

  %% Send message
  amqp_channel:cast(Channel, Publish, Msg).

%% @doc
%% Open a new connection, produce message on selected queue and close the connection.
%% @end
-spec(produce_once(Host :: string(), User :: binary(), Password :: binary(), ExchangeName :: binary(),
    ExchangeType :: binary(), RoutingKey :: binary(), Payload :: binary()) -> none()).
produce_once(Host, User, Password, ExchangeName, ExchangeType, RoutingKey, Payload) ->
  {ok, Connection, Channel} = start_connection(Host, User, Password),
  librarink_common_amqp:produce(Channel, ExchangeName, ExchangeType,  RoutingKey, Payload),
  librarink_common_amqp:close_connection(Connection, Channel).