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

execute(Exp, Res) :-
        empty_context(Env),
        execute(Exp, Env, _, Res).
        
execute(seq(E1, E2), Env0, Env, V) :-
        execute(E1, Env0, Env1, _V1),
        execute(E2, Env1, Env, V2),
        V = V2.
execute(let(Name, E), Env0, Env, V) :-
       execute(E, Env0, _, V),
       push_entry(Env0, Name, V, Env).
execute(if(C, T, E), Env, Env, V) :-
        execute(C, Env, _, CV),
        (  CV = true
        -> push_frame(Env, Env1), execute(T, Env1, _, V)
        ;  push_frame(Env, Env1), execute(E, Env1, _, V)
        ).
execute(int(N), Env, Env, N).
execute(real(N), Env, Env, N).
execute(param(Name), Env, Env, V) :-
        tm:parval(Name, V).
execute(var(Name), Env, Env, V) :-
        lookup_name(Env, Name, V).
execute(op(O, E1, E2), Env, Env, V) :-
        execute(E1, Env, _, V1),
        execute(E2, Env, _, V2),
        exec_op(O, V1, V2, V).

exec_op('+', V1, V2, V) :-
        V is V1 + V2.
exec_op('*', V1, V2, V) :-
        V is V1 * V2.
exec_op('<', V1, V2, V) :-
        ( V1 < V2 -> V = true ; V = false ).



empty_context([[]]).

push_frame(C, [[]|C]).

push_entry([F|C], Name, Type, [[entry(Name, Type)|F]|C]).

lookup_name([F|_], Name, Type) :-
        memberchk(entry(Name, Type), F), !.
lookup_name([_|C], Name, Type) :-
        lookup_name(C, Name, Type).



:- begin_tests(interp).

test(int) :-
        execute(int(5), 5).

test(param) :-
        execute(param('A001'), 5).

test(add) :-
        execute(op('+', int(1), int(2)), 3).

test(gt) :-
        execute(op('<', int(1), int(2)), true).

test(if) :-
        execute(if(op('<', int(1), int(2)), int(3), int(4)), 3).

:- end_tests(interp).