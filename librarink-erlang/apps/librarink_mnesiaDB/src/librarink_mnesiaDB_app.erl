%%%-------------------------------------------------------------------
%%% @doc
%%% librarink_mnesiaDB entry point. The application starts a supervisor
%%% that will manage gen_server and processes dynamically spawned
%%% to handle operation on MnesiaDB.
%%% @end
%%% Created : 05. set 2022 16:03
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

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
%% top supervisor of the tree. The only operations needed are to
%% install Mnesia DB creating schema and tables, then to launch the
%% MnesiaDB supervisor.
%% This is used also in case of takeover of the active node.
%% Taking over is the act of a dead node coming back from the dead,
%% being known to be more important than the backup nodes (maybe it
%% has better hardware), and deciding to run the application again.
%% This is usually done by gracefully terminating the backup
%% application and starting the main one instead.
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} |
{failover, node()}, StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {caught, thrown, E :: term()} |
  {caught, exited, E :: term()} |
  {caught, error, E :: term()} |
  {error, mnesiaDB_installation_failed, Result :: term()} |
  {error, Reason :: term()}).
start(StartType, _StartArgs) ->
  io:format("StartType: ~p~n", [StartType]),
  % Get active and backup nodes lists
  {ok, ActiveNodes} = application:get_env(librarink_mnesiaDB, active_nodes),
  {ok, BackupNodes} = application:get_env(librarink_mnesiaDB, backup_nodes),
  % Create mnesia DB schema and tables
  Result =
    try
      librarink_mnesiaDB_fun:install(ActiveNodes, BackupNodes)
    catch
      throw:E ->  {caught, thrown, E};
      exit:E ->   {caught, exited, E};
      error:E ->  {caught, error, E}
    end,

  case Result of
    {succeed, install_succeeded} ->
      librarink_mnesiaDB_sup:start_link();
    _ ->
      {error, mnesiaDB_installation_failed, Result}
  end.

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
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
