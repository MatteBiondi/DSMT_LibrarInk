%%%-------------------------------------------------------------------
%% @doc librarink_mqs top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(librarink_mqs_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1, start_child/1, stop_child/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 1,
                 period => 60},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

start_child(MasterPid) ->
  ChildSpec = #{
    id => MasterPid,
    start => {
      librarink_mqs, start_link, [MasterPid]
    },
    restart => permanent,
    shutdown => infinity,
    type => worker,
    modules => [librarink_mqs]
  },
  supervisor:start_child(?SERVER, ChildSpec).

stop_child(MasterPid) ->
  supervisor:terminate_child(librarink_mqs_sup, MasterPid),
  supervisor:delete_child(librarink_mqs_sup, MasterPid).