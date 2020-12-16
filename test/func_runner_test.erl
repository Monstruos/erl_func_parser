%%%-------------------------------------------------------------------
%%% @author monstruos
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. дек. 2020 23:07
%%%-------------------------------------------------------------------
-module(func_runner_test).
-author("monstruos").

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assertEqual(3, (func_runner:to_exec("X+1+Y"))(1, 1)).
