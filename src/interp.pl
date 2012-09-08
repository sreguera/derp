/*
  Copyright 2012 Jose Sebastian Reguera Candal

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
:- module(interp, [evaluate/2]).

:- use_module(tm).

/** <module> Interpreter

This module provides operations for interpreting an AST.

*/

evaluate(Exp, Res) :-
        initial_env(Env),
        evaluate(Exp, Env, Res).


%% evaluate(+Expression, +Environment, -Value)

evaluate(let(Name, Value, Body), Env0, Body_Value) :-
        evaluate(Value, Env0, Value_Value),
        put_env(Name, Env0, Value_Value, Env),
        evaluate(Body, Env, Body_Value).
evaluate(if(Condition, Then, Else), Env, If_Value) :-
        (  evaluate(Condition, Env, true)
        -> evaluate(Then, Env, If_Value)
        ;  evaluate(Else, Env, If_Value)
        ).
evaluate(int(Value), _Env, Value).
evaluate(real(Value), _Env, Value).
evaluate(param(Name), _Env, Value) :-
        tm:parval(Name, Value).
evaluate(var(Name), Env, Value) :-
        get_env(Name, Env, Value).
evaluate(op(Op, Left, Right), Env, Value) :-
        evaluate(Left, Env, Left_Value),
        evaluate(Right, Env, Right_Value),
        eval_op(Op, Left_Value, Right_Value, Value).
evaluate(fun(Name, Arg), Env, Value) :-
        evaluate(Arg, Env, Arg_Value),
        eval_fun(Name, Arg_Value, Value).

eval_fun(sin, Arg, V) :- V is sin(Arg).
eval_fun(cos, Arg, V) :- V is cos(Arg).
eval_fun(tan, Arg, V) :- V is tan(Arg).

eval_op(iadd, V1, V2, V) :- V is V1 + V2.
eval_op(radd, V1, V2, V) :- V is V1 + V2.
eval_op(isub, V1, V2, V) :- V is V1 - V2.
eval_op(rsub, V1, V2, V) :- V is V1 - V2.
eval_op(imul, V1, V2, V) :- V is V1 * V2.
eval_op(rmul, V1, V2, V) :- V is V1 * V2.
eval_op(idiv, V1, V2, V) :- V is div(V1, V2).
eval_op(rdiv, V1, V2, V) :- V is V1 / V2.
eval_op(ilt, V1, V2, V) :- ( V1 < V2 -> V = true ; V = false ).
eval_op(rlt, V1, V2, V) :- ( V1 < V2 -> V = true ; V = false ).
eval_op(igt, V1, V2, V) :- ( V1 > V2 -> V = true ; V = false ).
eval_op(rgt, V1, V2, V) :- ( V1 > V2 -> V = true ; V = false ).
eval_op(ieq, V1, V2, V) :- ( V1 =:= V2 -> V = true ; V = false ).
eval_op(req, V1, V2, V) :- ( V1 =:= V2 -> V = true ; V = false ).


put_env(Name, Env, Value, [entry(Name, Value)|Env]).

get_env(Name, Env, Value) :-
        memberchk(entry(Name, Value), Env).

initial_env(Env) :-
        setof(entry(Name, Value), var(Name, Value), Env).


%% var(Name, Value)

var('true', true).
var('false', false).
var('pi', Pi) :- Pi is pi.
var('e', E) :- E is e.


:- begin_tests(interp).

test(int) :-
        evaluate(int(5), 5).

test(param) :-
        evaluate(param('A001'), 5).

test(add) :-
        evaluate(op(iadd, int(1), int(2)), 3).

test(gt) :-
        evaluate(op(ilt, int(1), int(2)), true).

test(if) :-
        evaluate(if(op(ilt, int(1), int(2)), int(3), int(4)), 3).

:- end_tests(interp).