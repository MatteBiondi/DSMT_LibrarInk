%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. ago 2022 18:29
%%%-------------------------------------------------------------------
-module(websocket_handler).
-author("Federico").
-include("include/state.hrl").

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% Protocol upgrade
init(Req, _) ->
  %% TODO: get opts from state/config file
  TimeOut = 180 * 1000,
  FrameSize = 1024,
  Opts = #{idle_timeout => TimeOut, max_frame_size => FrameSize},
  {cowboy_websocket, Req, #websocket_state{}, Opts}.

%% Init websocket, hibernate save memory
websocket_init(State) ->
  io:format("[~p] Websocket init: ~p~n",[self(), State]),

  %% Register to MQS
  {ok, MqsPid} = librarink_mqs_client:register(self()),

  %% Create state
  {[], #websocket_state{mqs_pid = MqsPid}, hibernate}.

%% Messages received from user via websocket
websocket_handle({text, <<"keep-alive">>}, State) ->
  io:format("[~p] Websocket keep-alive~n",[self()]),
  %#state.objects = [], %% TODO: get from ws
  {ok, State, hibernate};
websocket_handle({text, Msg}, State) ->
  io:format("[~p] Websocket handle: ~p~n",[self(), Msg]),
  %#state.objects = [], %% TODO: get from ws
  {ok, State, hibernate};
websocket_handle(_, State) ->
  io:format("[~p] Websocket handle: unexpected message~n",[self()]),
  {ok, State, hibernate}.

%% TODO: define and add checks on Update format
%% The messages received from process handling the websocket are forwarded to user
websocket_info({update, Update}, State) ->
  io:format("[~p] Websocket info: update message~n",[self()]),
  {[{text, Update}], State, hibernate};
websocket_info({mqs,MqsPid}, State) when is_pid(MqsPid) ->
  io:format("[~p] New MqsPid~p~n",[self(), MqsPid]),
  ok = librarink_mqs_client:declare_queue(MqsPid, "update", "fanout", ""),
  Self = self(),
  ok = librarink_mqs_client:start_consumer(MqsPid, fun(Msg) -> Self ! {update, Msg} end),
  {[], State, hibernate};
websocket_info(close, State) ->
  io:format("[~p] Websocket info: close message~n",[self()]),
  {stop, State};
websocket_info(Msg, State) ->
  io:format("[~p] Websocket info: unexpected message~p~n",[self(),Msg]),
  {[], State, hibernate}.

%% Closing websocket
terminate(Reason, _Req, #websocket_state{}) ->
  io:format("[~p] Websocket terminate: ~p~n",[self(), Reason]),
  ok.
