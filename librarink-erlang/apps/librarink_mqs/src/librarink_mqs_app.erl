%%%-------------------------------------------------------------------
%% @doc
%% librarink_mqs entry point. The application starts a supervisor that will manage MQS processes dynamically spawned.
%% @end
%%%-------------------------------------------------------------------
-module(librarink_mqs_app).
-behaviour(application).

-export([start/2, stop/1]).
%% @doc
%% This function is called whenever an application is started using  application:start/[1,2], and should start the
%% processes of the application. If the application is structured according to the OTP design principles as a
%% supervision tree, this means starting the top supervisor of the tree.
%% The only operation needed is to launch the MQS supervisor.
%% @see librarink_mqs_sup
%% @end
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    {ok, Domain} = application:get_env(librarink_mqs, logger_domain),
    logger:set_primary_config(metadata, #{domain => Domain}),
    librarink_mqs_sup:start_link().

%% @doc
%% This function is called whenever an application has stopped. It is intended to be the opposite of Module:start/2
%% and should do any necessary cleaning up. The return value is ignored.
%% @end
-spec(stop(State :: term()) -> ok).
stop(_State) ->
    ok.
