%%%-------------------------------------------------------------------
%%% @doc
%%% Supervisor for worker process. The worker process need a {@link librarink_proxy_worker_bridge. bridge} to be
%%% supervised because id doesn't implements any OTP standard behaviour.
%%% @end
%%% Created : 05. set 2022 13:32
%%%-------------------------------------------------------------------
-module(librarink_proxy_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/4, stop_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(PROXY_WORKER_SUP, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================


%% @doc
%% Launch the supervisor process.
%% @end
-spec(start_link() -> {ok, Pid::pid()}).
start_link() ->
  supervisor:start_link({local, ?PROXY_WORKER_SUP}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc
%% Initialization of supervisor process. It doesn't start any child because they are spawned dynamically when requests
%% arrive.
%% @end
-spec(init(Args :: list()) -> {ok, {SupFlags :: term(), ChildSpecs :: list()}}).
init([]) ->
  MaxRestarts = 0,
  MaxSecondsBetweenRestarts = 1,
  SupFlags = #{
    strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts
  },
  {ok, {SupFlags, []}}.

%% @doc
%% Starts dynamically a new worker. The worker is not directly create by the supervisor, but by the spawned bridge.
%% @end
-spec(start_worker(From :: pid(), Tag :: term(), Request :: term(), Env :: term() ) ->
  {ok, Pid :: pid()} | {error, {already_started, Pid :: pid()}}).
start_worker(From, Tag, Request, Env) ->
  ChildSpec = #{
    id => Tag,
    start => {
      librarink_proxy_worker_bridge, start_link, [Request, From, Tag, Env]
    },
    restart => transient,
    shutdown => infinity,
    type => supervisor,
    modules => [librarink_proxy_worker_bridge]
  },
  supervisor:start_child(?PROXY_WORKER_SUP, ChildSpec).


%% @doc
%% Stops dynamically supervised child.
%% @end
-spec(stop_worker(ChildID :: atom()) -> none()).
stop_worker(ChildID) ->
  supervisor:terminate_child(?PROXY_WORKER_SUP, ChildID),
  supervisor:delete_child(?PROXY_WORKER_SUP, ChildID).
