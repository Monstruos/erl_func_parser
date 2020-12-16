-module(erl_func_parser_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    erl_func_parser_sup:start_link().

stop(_State) ->
    ok.
