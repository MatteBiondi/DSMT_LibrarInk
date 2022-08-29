%%%-------------------------------------------------------------------
%% @doc librarink public API
%% @end
%%%-------------------------------------------------------------------

-module(librarink_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    librarink_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
