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
:- module(sem, [analyze/2]).

:- use_module(db).

analyze(AST, AST2) :-
        empty_context(Env),
        analyze(AST, Env, _, _, AST2).

analyze(seq(E1, E2), Env0, Env, T, seq(E11, E22)) :-
        analyze(E1, Env0, Env1, _, E11),
        analyze(E2, Env1, Env, T, E22).
analyze(let(Name, E), Env0, Env, T, let(Name, E1)) :-
        analyze(E, Env0, _, T, E1),
        push_entry(Env0, Name, T, Env).
analyze(if(C, E1, E2), Env, Env, T, if(C1, E11, E22)) :-
        analyze(C, Env, _, CT, C1),
        (  CT = bool
        ;  throw(invalid_cond)
        ),
        push_frame(Env, Env1),
        analyze(E1, Env1, _, T1, E11),
        analyze(E2, Env1, _, T2, E22),
        (  T1 = T2
        -> T1 = T
        ;  throw(invalid_type)
        ).
analyze(int(X), Env, Env, int, int(X)).
analyze(real(X), Env, Env, real, real(X)).
analyze(value(Name), Env, Env, Type, Result) :-
        (  lookup_name(Env, Name, Type)
        -> Result = var(Name)
        ;  db:pardef(Name, Type)
        -> Result = param(Name)
        ;  throw(unknown_id)
        ).
analyze(op(O, E1, E2), Env, Env, T, op(O, E11, E22)) :-
        analyze(E1, Env, _, T1, E11),
        analyze(E2, Env, _, T2, E22),
        (  op(O, T1, T2, TR)
        -> T = TR
        ;  throw(invalid_op)
        ).

op('+', int, int, int).
op('+', real, real, real).
op('*', int, int, int).
op('*', real, real, real).
op('<', int, int, bool).
op('<', real, real, bool).


empty_context([[]]).

push_frame(C, [[]|C]).

push_entry([F|C], Name, Type, [[entry(Name, Type)|F]|C]).

lookup_name([F|_], Name, Type) :-
        memberchk(entry(Name, Type), F), !.
lookup_name([_|C], Name, Type) :-
        lookup_name(C, Name, Type).