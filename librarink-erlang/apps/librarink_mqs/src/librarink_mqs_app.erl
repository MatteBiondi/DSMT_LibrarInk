%%%-------------------------------------------------------------------
%% @doc librarink_mqs public API
%% @end
%%%-------------------------------------------------------------------

-module(librarink_mqs_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    librarink_mqs_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
