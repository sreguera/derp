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
:- module(interp, [execute/2]).

:- use_module(tm).

/** <module> Interpreter

This module provides operations for interpreting an AST.

*/

execute(Exp, Res) :-
        execute(Exp, [], Res).
        
execute(let(Name, Val, E), Env, V) :-
       execute(Val, Env, VV),
       execute(E, [entry(Name, VV)|Env], V).
execute(if(C, T, E), Env, V) :-
        execute(C, Env, CV),
        (  CV = true
        -> execute(T, Env, V)
        ;  execute(E, Env, V)
        ).
execute(int(N), _, N).
execute(real(N), _, N).
execute(param(Name), _, V) :-
        tm:parval(Name, V).
execute(var(Name), Env, V) :-
        memberchk(entry(Name, V), Env).
execute(op(O, E1, E2), Env, V) :-
        execute(E1, Env, V1),
        execute(E2, Env, V2),
        exec_op(O, V1, V2, V).

exec_op(iadd, V1, V2, V) :-
        V is V1 + V2.
exec_op(radd, V1, V2, V) :-
        V is V1 + V2.
exec_op(isub, V1, V2, V) :-
        V is V1 - V2.
exec_op(rsub, V1, V2, V) :-
        V is V1 - V2.
exec_op(imul, V1, V2, V) :-
        V is V1 * V2.
exec_op(rmul, V1, V2, V) :-
        V is V1 * V2.
exec_op(idiv, V1, V2, V) :-
        V is div(V1, V2).
exec_op(rdiv, V1, V2, V) :-
        V is V1 / V2.
exec_op(ilt, V1, V2, V) :-
        ( V1 < V2 -> V = true ; V = false ).
exec_op(rlt, V1, V2, V) :-
        ( V1 < V2 -> V = true ; V = false ).
exec_op(igt, V1, V2, V) :-
        ( V1 > V2 -> V = true ; V = false ).
exec_op(rgt, V1, V2, V) :-
        ( V1 > V2 -> V = true ; V = false ).
exec_op(ieq, V1, V2, V) :-
        ( V1 =:= V2 -> V = true ; V = false ).
exec_op(req, V1, V2, V) :-
        ( V1 =:= V2 -> V = true ; V = false ).

:- begin_tests(interp).

test(int) :-
        execute(int(5), 5).

test(param) :-
        execute(param('A001'), 5).

test(add) :-
        execute(op(iadd, int(1), int(2)), 3).

test(gt) :-
        execute(op(ilt, int(1), int(2)), true).

test(if) :-
        execute(if(op(ilt, int(1), int(2)), int(3), int(4)), 3).

:- end_tests(interp).