%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 01. set 2022 17:49
%%%-------------------------------------------------------------------
-module(librarink_mqs_amqp).
-author("Federico").

%% API
-export([start_connection/1, declare_queue/2, bind_queue/5, unbind_queue/4, start_consumer/3, consumer/3,
  close_connection/2]).
-include_lib("amqp_client/include/amqp_client.hrl").

start_connection(Host) ->
  % Connect to broker
  {ok, Connection} =
    amqp_connection:start(#amqp_params_network{host = Host}),
  % Open channel
  {ok, Channel} =
    amqp_connection:open_channel(Connection),
  {ok, Connection, Channel}.

declare_queue(Channel, QueueName)->

  % Declare queue
  #'queue.declare_ok'{queue = Queue} =
    amqp_channel:call(Channel, #'queue.declare'{queue=QueueName, exclusive = true}),
  {ok, [Queue]}.

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

start_consumer(_Channel, _Consumer, [], Tags) -> Tags;
start_consumer(Channel, Consumer, [Queue|Tail], Tags) ->
  #'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:subscribe(Channel, #'basic.consume'{queue = Queue}, Consumer),
  start_consumer(Channel, Consumer, Tail, [Tag] ++ Tags).
start_consumer({Connection, Channel}, Queues, Callback) ->
  Consumer = spawn_link(?MODULE, consumer, [Connection, Channel, Callback]),
  {ok, Consumer, start_consumer(Channel, Consumer, Queues, [])}.


%% @private
%% @doc Handling requests
-spec(unbind_queue(Channel :: pid(), Queue :: binary(),ExchangeName :: binary(), RoutingKey :: binary()) ->
  Ret :: #'queue.unbind_ok'{}).
unbind_queue(Channel, Queue, ExchangeName, RoutingKey) when is_binary(Queue), is_binary(ExchangeName), is_binary(RoutingKey) ->
  #'queue.unbind_ok'{}= amqp_channel:call(Channel, #'queue.unbind'{
    queue = Queue,
    exchange = ExchangeName,
    routing_key = RoutingKey
  }),
  ok.

close_connection(Connection, Channel) ->
  ok = amqp_channel:close(Channel),
  ok = amqp_connection:close(Connection).

consumer(Connection, Channel, Callback) ->
  process_flag(trap_exit, true),
  case is_process_alive(Channel) of
    true ->
      receive
        #'basic.consume_ok'{} ->
          io:format("[~p] Basic.consume_ok~n", [self()]),
          consumer(Connection, Channel, Callback);
        {#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Msg}} ->
          io:format("~p Consumer: ~p~n",[self(), Msg]),
          amqp_channel:cast(Channel, #'basic.ack'{delivery_tag = Tag}),
          Callback(Msg),
          consumer(Connection, Channel, Callback);
        #'basic.cancel_ok'{} ->
          io:format("[~p] Cancel~n",[self()]),
          cancel;
        {'EXIT',_Pid, Reason}  ->
          io:format("[~p] MQS process crashed: ~p~n",[self(), Reason]),
          close_connection(Connection, Channel);
        Any ->
          io:format("[~p] Unexpected message: ~p~n",[self(), Any])
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