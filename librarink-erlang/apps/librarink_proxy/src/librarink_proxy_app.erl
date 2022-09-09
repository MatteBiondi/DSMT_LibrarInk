%%%-------------------------------------------------------------------
%% @doc librarink_proxy public API
%% @end
%%%-------------------------------------------------------------------

-module(librarink_proxy_app).

-behaviour(application).

-export([start/2, stop/1]).
-define(GET_ENV(Param),element(2,application:get_env(librarink_proxy, Param))).

%% @doc
%% @end
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    librarink_proxy_sup:start_link().

%% @doc
%% @end
-spec(stop(State :: term()) -> ok).
stop(_State) ->
    gen_server:stop(?GET_ENV(proxy_name), shutdown, 180000). %% TODO: handle worker stop

%% internal functions
