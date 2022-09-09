%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing librarink_mnesia higher worker level supervisor.
%%% Launched by the top level supervisor, it has no default child
%%% supervised.
%%% @end
%%% Created : 06. set 2022 09:47
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/4, stop_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(_Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init(_Args) ->
  MaxRestarts = 1,
  MaxSecondsBetweenRestarts = 60,
  SupFlags = #{strategy => simple_one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  _Child = [],

  {ok, {SupFlags, _Child}}.


%% @private
%% @doc Start new supervised child, i.e. the librarink supervisor bridge associated
%% to a master process. The supervisor bridge will be used to supervise a worker.
-spec(start_worker(Function::atom(), Args::list(term()), BridgeSupId::string(), From::tuple()) -> {ok, Pid :: pid()}).
start_worker(Function, Args, BridgeSupId, From) ->
  ChildSpec = #{id => BridgeSupId,
    start => {librarink_mnesiaDB_bridge_sup, start_link, [BridgeSupId, Function, Args, From]},
    restart => transient,
    shutdown => infinity,
    type => supervisor,
    modules => ['librarink_mnesiaDB_bridge_sup']},
  supervisor:start_child(self(), ChildSpec).


%% @doc
%% Terminates or brutal_kills the child which is identified by the ChildID
%% @end
-spec(stop_worker(ChildID :: atom()) -> none()).
stop_worker(ChildID) ->
  supervisor:terminate_child(?SERVER, ChildID),
  supervisor:delete_child(?SERVER, ChildID).