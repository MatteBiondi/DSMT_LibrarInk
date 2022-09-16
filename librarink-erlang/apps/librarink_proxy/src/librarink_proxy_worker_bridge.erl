%%%-------------------------------------------------------------------
%%% @doc
%%% Module that acts as bridge between standard OTP supervisor and custom worker process that doesn't implements any
%%% OTP behaviours
%%% @end
%%% Created : 07. set 2022 17:44
%%%-------------------------------------------------------------------
-module(librarink_proxy_worker_bridge).

-behaviour(supervisor_bridge).

%% API
-export([start_link/4]).

%% Supervisor callbacks
-export([init/1, terminate/2]).


-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link(Request :: term(), From :: pid(), Tag :: reference(), Env :: term()) -> {ok, Pid :: pid()} | {error,
Reason :: term()}).
start_link(Request, From, Tag, Env) ->
  supervisor_bridge:start_link(?MODULE, [Request, From, Tag, Env]).

%%%===================================================================
%%% Supervisor_bridge callbacks
%%%===================================================================

%% @doc Starts the custom process and return it's pid. If that process will crash the supervisor will be notified
-spec(init(Args :: list()) -> {ok, Pid :: pid(),
  Opts :: none()}).
init([Request, From, Tag, Env]) ->
  WorkerPid = spawn(librarink_proxy_worker, work, [Request, From, Tag, Env]),
  {ok, WorkerPid, []}.

terminate(_Reason, []) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
