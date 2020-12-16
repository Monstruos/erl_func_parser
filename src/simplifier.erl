%%%-------------------------------------------------------------------
%%% @author monstruos
%%% @copyright (C) 2020, 
%%% @doc
%%%
%%% @end
%%% Created : 03. Сент. 2020 2:16
%%%-------------------------------------------------------------------
-module(simplifier).
-author("monstruos").

%% API
-export([simplify/1]).

simplify({Op, X, Y}) ->
   simplify_({Op, simplify_(X), simplify_(Y)});
simplify(X) -> X.

simplify_({'+', X, 0}) -> simplify(X);
simplify_({'+', 0, Y}) -> simplify(Y);
simplify_({'-', X, 0}) -> simplify(X);
simplify_({'-', 0, Y}) -> simplify(Y);
simplify_({'*', X, 1}) -> simplify(X);
simplify_({'*', 1, Y}) -> simplify(Y);
simplify_({'*', _, 0}) -> 0;
simplify_({'*', 0, _}) -> 0;
simplify_({'/', X, 1}) -> simplify(X);
simplify_({'^', 1, _}) -> 1;
simplify_({'^', X, 1}) -> simplify(X);
simplify_(X) -> X.