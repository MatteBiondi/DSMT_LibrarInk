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

%% API
-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% Protocol upgrade
init(Req, _) ->
  %% TODO: get opts from state/config file
  TimeOut = 300 * 1000,
  FrameSize = 1024,
  Opts = #{idle_timeout => TimeOut, max_frame_size => FrameSize},
  Objs = [{}], %% TODO: get from ws
  {cowboy_websocket, Req, Objs, Opts}.

%% Init websocket, hibernate save memory
websocket_init(State) ->
  io:format("[~p] Websocket init: ~p~n",[self(), State]),
  {[], State, hibernate}.

%% Messages received from user via websocket
websocket_handle({text, Msg}, State) ->
  io:format("[~p] Websocket handle: ~p~n",[self(), Msg]),
  {ok, State, hibernate};
websocket_handle(_, State) ->
  io:format("[~p] Websocket handle: unexpected message~n",[self()]),
  {ok, State, hibernate}.

%% Send messages
%% TODO: define and add checks on Update format
%% The messages received from process handling the websocket are forwarded to user
%% TODO: Find pid: Central repo ? MQS ?
websocket_info({update, Update}, State) ->
  io:format("[~p] Websocket info: update message~n",[self()]),
  {[{text, Update}], State, hibernate};
websocket_info(close, State) ->
  io:format("[~p] Websocket info: close message~n",[self()]),
  {stop, State};
websocket_info(_, State) ->
  io:format("[~p] Websocket info: unexpected message~n",[self()]),
  {[], State, hibernate}.

%% Closing websocket
terminate(Reason, _Req, _State) ->
  io:format("[~p] Websocket terminate: ~p~n",[self(), Reason]),
  ok.