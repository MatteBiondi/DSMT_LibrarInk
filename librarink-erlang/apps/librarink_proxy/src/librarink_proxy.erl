%%%-------------------------------------------------------------------
%%% @doc
%%% Module containing the definition of proxy server. It receive requests from clients that will be processed by a
%%% separate worker process.
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

%% @doc Initializes the server, environment variable are loaded into the server state.
%% Frequent accesses to the environment might be not optimized, record into state should be faster
%% @end
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

%% @doc Handling request messages sent by client. The proxy handle each request with a dedicated process that will
%% take care of sending response to the client.
%% @end
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()}, Env :: #librarink_proxy_env{}) ->
  {reply, Reply :: term(), NewState :: #librarink_proxy_env{}} |
  {stop, Reason :: term(), NewState :: #librarink_proxy_env{}}).
handle_call(Request, {From, Tag}, Env) ->
  io:format("[~p] Incoming request: ~p~n",[self(), {From, Tag, Request}]),
  case librarink_proxy_worker_sup:start_worker(From, Tag, Request, Env) of
    {ok, _Pid} -> {noreply, Env}; %% The response will be sent by the worker process
    {error, {already_started, _Pid}}-> {noreply, Env}; %% The worker process is already handling such request,
    %% something strange happened, but the request will not be processed twice
    _ -> {reply, error, Env}
  end.

%% @private
%% @doc Handling cast messages, messages are ignored.
-spec(handle_cast(Request :: term(), Env :: #librarink_proxy_env{}) ->
  {noreply, NewState :: #librarink_proxy_env{}}).
handle_cast(_Request, Env) ->
  {noreply, Env}.

%% @doc Handling request messages sent by client. The proxy handle each request with a dedicated process that will
%% take care of sending response to the client.
%% @end
-spec(handle_info({From :: pid(), Tag :: reference(), Request :: term()}, Env :: #librarink_proxy_env{}) ->
  {noreply, NewState :: #librarink_proxy_env{}}).
handle_info({From, Tag, Request}, Env) when is_pid(From) ->
  io:format("[~p] Incoming request: ~p~n",[self(), {From, Tag, Request}]),
  case librarink_proxy_worker_sup:start_worker(From, Tag, Request, Env) of
    {ok, _Pid} -> {noreply, Env}; %% The response will be sent by the worker process
    {error, {already_started, _Pid}}-> {noreply, Env}; %% The worker process is already handling such request,
                                          %% something strange happened, but the request will not be processed twice
    _ -> {noreply, Env}
  end.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    Env :: #librarink_proxy_env{}) -> term()).
terminate(_Reason, _Env) ->
  ok.
