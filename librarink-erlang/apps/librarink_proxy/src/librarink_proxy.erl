%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%% Created : 05. set 2022 18:06
%%%-------------------------------------------------------------------
-module(librarink_proxy).


-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(GET_ENV(Param),element(2,application:get_env(librarink_proxy, Param))).

-record(librarink_proxy_env, {mnesia_name, mnesia_nodes, request_timeout, mqs_host, exchange_type, routing_key}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?GET_ENV(proxy_name)}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(Args :: term()) -> {ok, Env :: #librarink_proxy_env{}} | {stop, Reason :: term()}).
init([]) ->
  Env = #librarink_proxy_env{
    mnesia_name = ?GET_ENV(mnesia_name),
    mnesia_nodes = ?GET_ENV(mnesia_nodes),
    request_timeout = ?GET_ENV(request_timeout),
    mqs_host = ?GET_ENV(mqs_host),
    exchange_type = ?GET_ENV(exchange_type),
    routing_key = ?GET_ENV(routing_key)
  },
  {ok, Env}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, Env :: #librarink_proxy_env{}) ->
  {reply, Reply :: term(), NewState :: #librarink_proxy_env{}} |
  {stop, Reason :: term(), NewState :: #librarink_proxy_env{}}).
handle_call(Request, {From, Tag}, Env) ->
  case librarink_proxy_worker_sup:start_worker(From, Tag, Request, Env) of
    {ok, _Pid} -> {noreply, Env};
    {error, {already_started, Pid}}-> {reply, Pid, Env};%%TODO
    _ -> {reply, error, Env}
  end.

%% @private
%% @doc Handling cast messages
-spec(handle_cast(Request :: term(), Env :: #librarink_proxy_env{}) ->
  {noreply, NewState :: #librarink_proxy_env{}}).
handle_cast(_Request, Env) ->
  {noreply, Env}.

%% @private
%% @doc Handling all non call/cast messages
-spec(handle_info(Info :: timeout() | term(), Env :: #librarink_proxy_env{}) ->
  {noreply, NewState :: #librarink_proxy_env{}} |
  {stop, Reason :: term(), NewState :: #librarink_proxy_env{}}).
handle_info(_Info, Env) ->
  {noreply, Env}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    Env :: #librarink_proxy_env{}) -> term()).
terminate(_Reason, _Env) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
