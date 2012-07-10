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

execute(seq(E1, E2), V) :-
        execute(E1, _V1),
        execute(E2, V2),
        V = V2.
execute(num(N), N).
execute(op(O, E1, E2), V) :-
        execute(E1, V1),
        execute(E2, V2),
        exec_op(O, V1, V2, V).

exec_op('+', V1, V2, V) :-
        V is V1 + V2.
exec_op('*', V1, V2, V) :-
        V is V1 * V2.
exec_op('<', V1, V2, V) :-
        ( V1 < V2 -> V = true ; V = false ).


:- begin_tests(interp).

test(num) :-
        execute(num(5), 5).

test(add) :-
        execute(op('+', num(1), num(2)), 3).

test(gt) :-
        execute(op('<', num(1), num(2)), true).

:- end_tests(interp).