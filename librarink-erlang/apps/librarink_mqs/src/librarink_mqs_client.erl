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
-export([register/1, declare_queue/4, start_consumer/2]).

register(MasterPid) ->
  librarink_mqs_sup:start_child(MasterPid).

declare_queue(Server, ExchangeName, ExchangeType, RoutingKey) ->
  gen_server:call(Server,{declare_queue, ExchangeName, ExchangeType, RoutingKey}).

start_consumer(Server, Callback) ->
  gen_server:call(Server,{start_consumer, Callback}).