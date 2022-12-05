%%%-------------------------------------------------------------------
%%% @doc
%%% Module implementing librarink_mnesia gen_server that receives all the
%%% requests from the proxy and asks the worker supervisor to fetch a
%%% new process to handle them
%%% @end
%%% Created : 06. set 2022 23:35
%%%-------------------------------------------------------------------
-module(librarink_mnesiaDB).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-record(librarink_mnesiaDB_state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Spawns the server and registers the local name (unique)
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @private
%% @doc Initializes the server
-spec(init(_Args :: term()) ->
  {ok, State :: #librarink_mnesiaDB_state{}} | {ok, State :: #librarink_mnesiaDB_state{}, timeout() | hibernate} |
  {stop, Reason :: term()} | ignore).
init(_Args) ->
  {ok, #librarink_mnesiaDB_state{}}.

%% @private
%% @doc Handling call messages
-spec(handle_call(Request :: term() | {Function::atom(), Args::map()}, From :: {pid(), Tag :: term()},
    State :: #librarink_mnesiaDB_state{}) ->
  {reply, Reply :: term(), NewState :: #librarink_mnesiaDB_state{}} |
  {reply, Reply :: term(), NewState :: #librarink_mnesiaDB_state{}, timeout() | hibernate} |
  {noreply, NewState :: #librarink_mnesiaDB_state{}} |
  {noreply, NewState :: #librarink_mnesiaDB_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), Reply :: term(), NewState :: #librarink_mnesiaDB_state{}} |
  {stop, Reason :: term(), NewState :: #librarink_mnesiaDB_state{}}).
handle_call({Function, Args}, From, State = #librarink_mnesiaDB_state{}) ->
  ?LOG_DEBUG("[~p] Handle call: ~p~n", [self(), {Function, Args}]),
  ?LOG_DEBUG("[~p] From: ~p~n", [self(), From]),
  {_, Tag} = From,
  librarink_mnesiaDB_worker_sup:start_worker(Function, Args, Tag, From),
  {noreply, State};
handle_call(Request, _From, State = #librarink_mnesiaDB_state{}) ->
  ?LOG_DEBUG("[~p] Unexpected request. Handle cast: ~p~n", [self(), Request]),
  {reply, {error, invalid_call_request}, State}.

%% @private
%% @doc Handling cast messages. This semantic is not used for the application.
-spec(handle_cast(Request :: term(), State :: #librarink_mnesiaDB_state{}) ->
  {noreply, NewState :: #librarink_mnesiaDB_state{}} |
  {noreply, NewState :: #librarink_mnesiaDB_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mnesiaDB_state{}}).
handle_cast(Request, State = #librarink_mnesiaDB_state{}) ->
  ?LOG_DEBUG("[~p] Unexpected request. Handle cast: ~p~n", [self(), Request]),
  {noreply, State}.

%% @private
%% @doc Handling all non call/cast messages. This semantic is not used for the application.
-spec(handle_info(Info :: timeout() | term(), State :: #librarink_mnesiaDB_state{}) ->
  {noreply, NewState :: #librarink_mnesiaDB_state{}} |
  {noreply, NewState :: #librarink_mnesiaDB_state{}, timeout() | hibernate} |
  {stop, Reason :: term(), NewState :: #librarink_mnesiaDB_state{}}).
handle_info(Info, State = #librarink_mnesiaDB_state{}) ->
  ?LOG_DEBUG("[~p] Unexpected request. Handle info: ~p~n", [self(), Info]),
  {noreply, State}.

%% @private
%% @doc This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #librarink_mnesiaDB_state{}) -> term()).
terminate(Reason, _State = #librarink_mnesiaDB_state{}) ->
  ?LOG_NOTICE("[~p] Termination: ~p~n", [self(), Reason]),
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
