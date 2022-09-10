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
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1, terminate/2]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor bridge
-spec(start_link( Function::atom(), Args::map(), From::tuple()) ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Function, Args, From) ->
  supervisor_bridge:start_link(?MODULE, [Function, Args, From]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to start the subsystem and initialize.
-type user_request_type() :: {struct, [{Function::atom(), Args::map(), From::tuple()}]}.
-spec(init(ComposedArg :: user_request_type()) -> {ok, Pid::pid(), []}).
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
