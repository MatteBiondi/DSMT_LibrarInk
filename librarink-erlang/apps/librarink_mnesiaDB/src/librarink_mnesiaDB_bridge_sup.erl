%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing librarink_mnesia worker level supervisor.
%%% It has a default child supervised.
%%% @end
%%% Created : 07. set 2022 19:44
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_bridge_sup).

-behaviour(supervisor_bridge).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor bridge
-spec(start_link( BridgeSupId::string(), Function::atom(), Args::list(term()), From::tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(BridgeSupId, Function, Args, From) ->
  supervisor:start_link({local, list_to_atom("Bridge-"++BridgeSupId)}, ?MODULE, [Function, Args, From]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to start the subsystem and initialize.
-spec(init(list(Function::atom(), Args::list(term()), From::tuple())) -> {ok, Pid::pid(), []}).
init([Function, Args, From]) ->
  Pid = spawn(librarink_mnesiaDB_worker, handle_request, [Function, Args,From]),
  {ok, Pid, []}.


%% @private
%% @doc This function is called by the supervisor bridge when it is
%% about to terminate. It is to be the opposite of Module:init/1 and
%% stop the subsystem and do any necessary cleaning up. The return
%% value is ignored.
terminate(_Reason, []) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================
