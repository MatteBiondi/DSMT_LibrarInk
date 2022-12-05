%%%-------------------------------------------------------------------
%% @doc
%% The application listen for client requests and forward them to the current active server that will generate the
%% response. The response is sent to the proxy and the forwarder to the original client. The mechanism is transparent
%% to the client, so this level allows to change the organization of logic server without any changes to the client.
%% @see librarink_mnesiaDB
%% @end
%%%-------------------------------------------------------------------

-module(librarink_proxy_app).

-behaviour(application).

-export([start/2, stop/1]).
-define(GET_ENV(Param),element(2,application:get_env(librarink_proxy, Param))).

%% @doc
%% Start the application instantiating the proxy supervisor and the supervisor that will take care of spawn
%% dynamically new workers
%% @end
-spec(start(StartType :: normal | {takeover, node()}, StartArgs :: term()) ->
    {ok, pid()} | {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    {ok, Domain} = application:get_env(librarink_proxy, logger_domain),
    logger:set_primary_config(metadata, #{domain => Domain}),
    librarink_proxy_sup:start_link().

%% @doc
%% Stop the application and all its components
%% @end
-spec(stop(State :: term()) -> ok).
stop(_State) ->
    gen_server:stop(?GET_ENV(proxy_name), shutdown, 180000).