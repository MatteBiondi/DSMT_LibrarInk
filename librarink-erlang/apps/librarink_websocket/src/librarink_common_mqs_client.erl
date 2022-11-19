%%%-------------------------------------------------------------------
%%% @doc
%%% Client module to make requests to the process connected to the Message Queuing Systems. The module make easier
%%% the interaction with the MQS process, details are hidden to the client process.
%%% @end
%%% Created : 31. ago 2022 12:56
%%%-------------------------------------------------------------------
-module(librarink_common_mqs_client).

%% API
-export([register/1, declare_queue/2, bind_queue/5, unbind_queue/4, start_consumer/2, unregister/1]).

%% TODO: check no proc and error return
%%% @doc
%%% Request to the MQS supervisor to start a MQS process that will interact with the Message Queuing Systems adopting
%%% AMQP protocol, establishing a connection with the system.
%%% @end
-spec(register(MasterPid :: pid()) -> ok | error).
register(MasterPid) ->
  try
    librarink_mqs_sup:start_child(MasterPid)
  catch exit:_Error ->
    error
  end.

%%% @doc
%%% Request to declare a queue on which the MQS process will receive messages.
%%% @end
-spec(declare_queue(Server :: pid(), QueueName :: binary()) -> ok | error).
declare_queue(Server, QueueName) ->
  try
    gen_server:call(Server, {declare_queue, QueueName})
  catch exit:_Error ->
    error
  end.

%%% @doc
%%% Request to bind the specified queue to an exchange that will be declared using the passed parameters.
%%% @end
-spec(bind_queue(Server :: pid(), QueueName :: binary(), ExchangeName :: binary(),
    ExchangeType :: binary(), RoutingKey :: binary()) -> ok | error).
bind_queue(Server, QueueName, ExchangeName, ExchangeType, RoutingKey) ->
  try
    gen_server:call(Server, {bind_queue, QueueName, ExchangeName, ExchangeType, RoutingKey})
  catch exit:_Error ->
    error
  end.

%%% @doc
%%% Request to start consume the messages received on queues declared and apply the given callback function to the
%%% received messages
%%% @end
-spec(start_consumer(Server :: pid(), Callback :: mfa()) -> ok | error).
start_consumer(Server, Callback) ->
  try
    gen_server:call(Server, {start_consumer, Callback})
  catch exit:_Error ->
    error
  end.

%%% @doc
%%% Request to unbind the specified queue to the exchange passed as argument.
%%% @end
-spec(unbind_queue(Server :: pid(), QueueName :: binary(), ExchangeName :: binary(),
    RoutingKey :: binary()) -> ok | error).
unbind_queue(Server, QueueName, ExchangeName, RoutingKey) ->
  try
    gen_server:call(Server, {unbind_queue, QueueName, ExchangeName, RoutingKey})
  catch exit:_Error ->
    error
  end.

%%% @doc
%%% Request to MQS supervisor to stop the associated process and close connection with Message Queuing Systems.
%%% @end
-spec(unregister(MasterPid :: pid()) -> none()).
unregister(MasterPid) ->
  librarink_mqs_sup:stop_child(MasterPid).