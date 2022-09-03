%%%-------------------------------------------------------------------
%% @doc
%% Module implementing librarink_mqs top level supervisor. Launched at application startup with no default child
%% supervised.
%% @end
%%%-------------------------------------------------------------------

-module(librarink_mqs_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1, start_child/1, stop_child/1]).

-define(SERVER, ?MODULE).

%% @doc
%% Start function used to spawn the supervisor that will be register in order to receive commands by other processes.
%% @end
-spec(start_link() -> {ok, Pid::pid()}).
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
%% @doc
%% Initialization of supervisor process. The initial configuration doesn't include any children, they will be spawned
%% dynamically in response to the received requests. In case of crash of supervised process it will replaces with
%% another process, following the one-for-one semantic.
%% @end
-spec(init(Args :: list()) -> {ok, {SupFlags :: term(), ChildSpecs :: list()}}).
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 300},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.

%% @doc
%% Start new supervised child, i.e. the MQS process associated to a master process that want to interact with message
%% queuing system broker. The MQS process is identified by the master process pid, that also represents an argument
%% passed to the start function. In case of crash the MQS process is restarted.
%% @see librarink_mqs
%% @end
-spec(start_child(MasterPid :: pid()) -> {ok, Pid :: pid()}).
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

%% @doc
%% Stop and destroy process child identified by the input argument.
%% @end
-spec(stop_child(MasterPid :: pid()) -> none()).
stop_child(MasterPid) ->
  supervisor:terminate_child(librarink_mqs_sup, MasterPid),
  supervisor:delete_child(librarink_mqs_sup, MasterPid).