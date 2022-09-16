%%%-------------------------------------------------------------------
%% @doc Module defining librarink_proxy top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(librarink_proxy_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

%% @doc
%% Spawn proxy supervisor that will supervise the server and will restart it in case of crash.
%% @end
-spec(start_link() -> {ok, Pid::pid()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% @doc
%% Initialization of supervisor process. The initial configuration include two children:
%% <ul>
%%  <li>{@link librarink_proxy. Proxy}: server that receive requests from client and schedule them to a dedicated
%% worker</li>
%%  <li>{@link librarink_proxy_worker_sup. Proxy worker supervisor}: supervisor process that is spawned to handle
%% dynamically spawning of worker process</li>
%% </ul>
%% In case of crash of supervised process it will replaces with another process, following the one-for-one semantic.
%% @end
-spec(init(Args :: list()) -> {ok, {SupFlags :: term(), ChildSpecs :: list()}}).
init([]) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 3600},
    ChildSpecs = [
      #{
        id => librarink_proxy,
        start => {
          librarink_proxy, start_link, []
        },
        restart => permanent,
        shutdown => infinity,
        type => worker,
        modules => [librarink_proxy]
      },
      #{
        id => librarink_proxy_worker_sup,
        start => {
          librarink_proxy_worker_sup, start_link, []
        },
        restart => permanent,
        shutdown => infinity,
        type => supervisor,
        modules => [librarink_proxy_worker_sup]
      }
    ],
    {ok, {SupFlags, ChildSpecs}}.
