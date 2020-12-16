%%%-------------------------------------------------------------------
%%% @author monstruos
%%% @copyright (C) 2020, 
%%% @doc
%%%
%%% @end
%%% Created : 02. Сент. 2020 23:06
%%%-------------------------------------------------------------------
-module(func_runner).
-author("monstruos").

%% API
-export([
    parse/1,
    read/1,
    to_exec/1
]).


parse([_|_] = L) ->
    RPN = read(list_to_binary(L), [], [], false),
    Res = convert(RPN),
    simplifier:simplify(Res);
parse(F) -> F.

read(L) -> read(list_to_binary(L), [], [], false).

read(<<$x, Body/binary>>, R, Op, _) -> read(Body, ['x' | R], Op, true);
read(<<$X, Body/binary>>, R, Op, _) -> read(Body, ['x' | R], Op, true);
read(<<$y, Body/binary>>, R, Op, _) -> read(Body, ['y' | R], Op, true);
read(<<$Y, Body/binary>>, R, Op, _) -> read(Body, ['y' | R], Op, true);
read(<<Bin, Body/binary>>, R, Op, true) when Bin >= $0, Bin =< $9 -> read(Body, append(Bin, R), Op, true);
read(<<$., Body/binary>>, R, Op, true) -> read(Body, append($., R), Op, true);
read(<<Bin, Body/binary>>, R, Op, false) when Bin >= $0, Bin =< $9 -> read(Body, [{int, Bin - $0} | R], Op, true);

read(<<$-, Body/binary>>, R, Op, false) -> read(Body, ['u-' | R], Op, false);
read(<<$-, _/binary>>, ['u-' | _], _, _) -> error(badarg);
read(<<$-, _/binary>> = Body, R, [OH | Op], Pr) when OH =/= '(' -> read(Body, [OH | R], Op, Pr);
read(<<$-, Body/binary>>, R, Op, true) -> read(Body, R, ['-' | Op], false);

read(<<$+, _/binary>> = Body, R, [OH | Op], Pr) when OH =/= '(' -> read(Body, [OH | R], Op, Pr);
read(<<$+, Body/binary>>, R, Op, true) -> read(Body, R, ['+' | Op], false);

read(<<$*, _/binary>> = Body, R, [OH | Op], Pr) when OH == '/'; OH == '*'; OH == '^' -> read(Body, [OH | R], Op, Pr);
read(<<$*, Body/binary>>, R, Op, true) -> read(Body, R, ['*' | Op], false);

read(<<$/, _/binary>> = Body, R, [OH | Op], Pr) when OH == '/'; OH == '*'; OH == '^' -> read(Body, [OH | R], Op, Pr);
read(<<$/, Body/binary>>, R, Op, true) -> read(Body, R, ['/' | Op], false);

read(<<$^, _/binary>> = Body, R, ['^' | Op], Pr) -> read(Body, ['^' | R], Op, Pr);
read(<<$^, Body/binary>>, R, Op, true) -> read(Body, R, ['^' | Op], false);

read(<<$(, Body/binary>>, R, Op, _) -> read(Body, R, ['(' | Op], false);
read(<<$), Body/binary>>, R, Op, true) -> {R2, Op2} = push(R, Op), read(Body, R2, Op2, true);
read(<<>>, R, Op, _) -> prepare(R) ++ Op.

push(R, ['(' | Op]) -> {R, Op};
push(R, [OH | Op]) -> push([OH | R], Op).


prepare(L) -> prepare(L, []).

prepare([], Acc) -> Acc;
prepare([{int, Int} | S], Acc) -> prepare(S, [Int | Acc]);
prepare([{float, _, Num} | S], Acc) -> prepare(S, [Num | Acc]);
prepare([E | S], Acc) -> prepare(S, [E | Acc]).

append($., [{int, Int} | S]) -> [{float, 0.1, Int} | S];
append(Bin, [{int, Int} | S]) -> [{int, Int*10 + Bin - $0} | S];
append(Bin, [{float, Mul, Fl} | S]) -> [{float, Mul / 10, Fl + Mul * (Bin - $0)} | S];
append(Bin, S) -> [{int, Bin - $0} | S].

convert(L) -> convert(L, []).

convert(['u-' | L], [Num | S]) -> convert(L, [{'*', -1, Num} | S]);
convert(['+' | L], [X, Y | S]) -> convert(L, [{'+', Y, X} | S]);
convert(['-' | L], [X, Y | S]) -> convert(L, [{'-', Y, X} | S]);
convert(['*' | L], [X, Y | S]) -> convert(L, [{'*', Y, X} | S]);
convert(['/' | L], [X, Y | S]) -> convert(L, [{'/', Y, X} | S]);
convert(['^' | L], [X, Y | S]) -> convert(L, [{'^', Y, X} | S]);
convert([Num | L], S) -> convert(L, [Num | S]);
convert([], [S]) -> S.

to_exec([_|_] = Ex) -> to_exec(parse(Ex));
to_exec({_, _, _} = Ex) -> fun(X, Y) when is_number(X), is_number(Y) -> run(Ex, X, Y) end;
to_exec(Fn) -> Fn.

run({Op, 'x', Snd}, X, Y) -> run({Op, X, Snd}, X, Y);
run({Op, 'y', Snd}, X, Y) -> run({Op, Y, Snd}, X, Y);
run({Op, Fst, 'x'}, X, Y) -> run({Op, Fst, X}, X, Y);
run({Op, Fst, 'y'}, X, Y) -> run({Op, Fst, Y}, X, Y);
run({Op, Fst, Snd}, X, Y) -> exec(Op, run(Fst, X, Y), run(Snd, X, Y));
run(N, _, _) -> N.

exec('+', A, B) -> A + B;
exec('-', A, B) -> A - B;
exec('*', A, B) -> A * B;
exec('/', A, B) -> A / B;
exec('^', A, B) -> math:pow(A, B).