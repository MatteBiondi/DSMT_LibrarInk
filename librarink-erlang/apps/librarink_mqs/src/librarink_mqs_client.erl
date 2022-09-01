%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. ago 2022 12:56
%%%-------------------------------------------------------------------
-module(librarink_mqs_client).
-author("Federico").

%% API
-export([register/1, declare_queue/2, bind_queue/5, start_consumer/2, unbind_queue/4, unregister/1]).

%% TODO: check no proc and error return
register(MasterPid) ->
  librarink_mqs_sup:start_child(MasterPid).

declare_queue(Server, QueueName) ->
  try
    gen_server:call(Server, {declare_queue, QueueName})
  catch exit:_Error ->
    error
  end.

bind_queue(Server, QueueName, ExchangeName, ExchangeType, RoutingKey) ->
  try
    gen_server:call(Server, {bind_queue, QueueName, ExchangeName, ExchangeType, RoutingKey})
  catch exit:_Error ->
    error
  end.

start_consumer(Server, Callback) ->
  try
    gen_server:call(Server, {start_consumer, Callback})
  catch exit:_Error ->
    error
  end.

unbind_queue(Server, QueueName, ExchangeName, RoutingKey) ->
  try
    gen_server:call(Server, {unbind_queue, QueueName, ExchangeName, RoutingKey})
  catch exit:_Error ->
    error
  end.

unregister(MasterPid) ->
  librarink_mqs_sup:stop_child(MasterPid).