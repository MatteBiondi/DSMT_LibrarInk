%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 07. set 2022 17:44
%%%-------------------------------------------------------------------
-module(librarink_proxy_worker_bridge).

-behaviour(supervisor_bridge).

%% API
-export([start_link/3]).

%% Supervisor callbacks
-export([init/1, terminate/2]).


-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(Request :: term(), Client :: term(), Env :: term()) -> {ok, Pid :: pid()} | {error, Reason :: term()}).
start_link(Request, Client, Env) ->
  supervisor_bridge:start_link(?MODULE, [Request, Client, Env]).

%%%===================================================================
%%% Supervisor_bridge callbacks
%%%===================================================================

init([Request, Client, Env]) ->
  WorkerPid = spawn(librarink_proxy_worker, work, [Request, Client, Env]),
  {ok, WorkerPid, []}.

terminate(_Reason, []) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
