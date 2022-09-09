%%%-------------------------------------------------------------------
%%% @doc
%%%
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
%% @end
-spec(start_link() -> {ok, Pid::pid()}).

start_link() ->
  supervisor:start_link({local, ?PROXY_WORKER_SUP}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @doc
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
%% @end
-spec(start_worker(From :: pid(), Tag :: term(),Request :: term(), Env :: term() ) ->
  {ok, Pid :: pid()} | {error, {already_started, Pid :: pid()}}).
start_worker(From, Tag, Request, Env) ->
  ChildSpec = #{
    id => Tag,
    start => {
      librarink_proxy_worker_bridge, start_link, [Request, {From, Tag}, Env]
    },
    restart => transient,
    shutdown => infinity,
    type => supervisor,
    modules => [librarink_proxy_worker_bridge]
  },
  supervisor:start_child(?PROXY_WORKER_SUP, ChildSpec).


%% @doc
%% @end
-spec(stop_worker(ChildID :: atom()) -> none()).
stop_worker(ChildID) ->
  supervisor:terminate_child(?PROXY_WORKER_SUP, ChildID),
  supervisor:delete_child(?PROXY_WORKER_SUP, ChildID).
