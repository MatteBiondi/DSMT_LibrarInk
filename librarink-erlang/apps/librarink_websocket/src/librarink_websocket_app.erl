%%%-------------------------------------------------------------------
%%% @doc
%%% Entry point of librarink_websocket application. The application launch the
%%% <a href="https://ninenines.eu" target="_blank"> Cowboy</a> webserver and define a websocket endpoint associated
%%% with {@link librarink_websocket_handler. websocket handler}.
%%% @end
%%% Created : 27. ago 2022 18:28
%%%-------------------------------------------------------------------
-module(librarink_websocket_app).
-behaviour(application).

%% Application callbacks
-export([start/2,stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application is started using  application:start/[1,2], and should start the
%% processes of the application. If the application is structured according to the OTP design principles as a
%% supervision tree, this means starting the top supervisor of the tree. The method define a new endpoint and bind it
%% to the handler module and then launch a new instance of cowboy webserver.
%% @see librarink_websocket_handler
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    {ok, WSPort} = application:get_env(librarink_websocket, port),
    {ok, Domain} = application:get_env(librarink_websocket, logger_domain),
    logger:set_primary_config(metadata, #{domain => Domain}),
    %% Start cowboy server
    UpdateDispatch = cowboy_router:compile([
        {'_', [{"/update", librarink_websocket_handler, []}]} %% {HostMatch, list({PathMatch, Handler, InitialState})}
    ]),

    cowboy:start_clear(update_listener,
        [{port, WSPort}],
        #{env => #{dispatch => UpdateDispatch}}
    ).


%%--------------------------------------------------------------------
%% @doc
%% This function is called whenever an application has stopped. It is intended to be the opposite of Module:start/2
%% and should do any necessary cleaning up. The return value is ignored. The cleaning operations include webserver
%% shutdown.
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    cowboy:stop_listener(update_listener).
