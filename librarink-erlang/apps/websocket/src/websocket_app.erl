%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. ago 2022 18:28
%%%-------------------------------------------------------------------
-module(websocket_app).

-author("Federico").

-behaviour(application).

%% Application callbacks
-export([start/2,stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->

    WSPort = 5000, %% TODO: configuration from config file

    %% Start cowboy server
    UpdateDispatch = cowboy_router:compile([
        {'_', [{"/update", websocket_handler, []}]} %% {HostMatch, list({PathMatch, Handler, InitialState})}
    ]),

    {ok, _} = cowboy:start_clear(update_listener,
        [{port, WSPort}],
        #{env => #{dispatch => UpdateDispatch}}
    ).

    %% Start application
    %%case websocket_sup:start_link() of
    %%    {ok, Pid} ->
    %%        {ok, Pid};
    %%    Error ->
    %%        Error
    %%end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok = cowboy:stop_listener(update_listener).

%%%===================================================================
%%% Internal functions
%%%===================================================================