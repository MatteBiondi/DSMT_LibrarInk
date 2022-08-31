%%%-------------------------------------------------------------------
%%% @author Federico
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. ago 2022 15:54
%%%-------------------------------------------------------------------
-author("Federico").
-record(websocket_state, {mqs_pid = none, objects=[]}).
-define(WEBSOCKET_STATE(Objects), #websocket_state{objects = Objects}).