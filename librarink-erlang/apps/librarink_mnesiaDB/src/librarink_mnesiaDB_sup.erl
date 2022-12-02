%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing librarink_mnesia top level supervisor.
%%% @end
%%% Created : 05. set 2022 11:13
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor that will be register in order to receive
%% commands by other processes.
-spec(start_link() -> {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about the
%% restart strategy, maximum restart frequency and child
%% specifications. Initialization of supervisor process. The initial
%% configuration includes two children spawned:
%% 1) the mnesiaDB gen_server to handle incoming request
%% 2) the supervisor that handles the worker processes.
%% In case of a crash of supervised processes, they will be replaced with others,
%% following the one-for-one semantic.
-spec(init(_Args :: term()) ->
  {ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ignore | {error, Reason :: term()}).
init(_Args) ->
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 3600,
  SupFlags = #{strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts},

  ChildSpecs = [
    #{id => 'mnesiaDB_server',
      start => {'librarink_mnesiaDB', start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => ['librarink_mnesiaDB']
    },
    #{id => 'mnesiaDB_worker_sup',
      start => {'librarink_mnesiaDB_worker_sup', start_link, []},
      restart => permanent,
      shutdown => infinity,
      type => supervisor,
      modules => ['librarink_mnesiaDB_worker_sup']
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
