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

/** <module> Semantic Analyzer

This module provides operations for converting an AST to a checked
AST.

*/

analyze(AST, AST2) :-
        analyze(AST, [], _, AST2).

analyze(let(Name, V, E), Env, T, let(Name, V1, E1)) :-
        analyze(V, Env, VT, V1),
        analyze(E, [entry(Name, VT)|Env], T, E1).
analyze(if(C, E1, E2), Env, T, if(C1, E11, E22)) :-
        analyze(C, Env, CT, C1),
        (  CT = bool, !
        ;  throw(invalid_cond)
        ),
        analyze(E1, Env, T1, E11),
        analyze(E2, Env, T2, E22),
        (  T1 = T2
        -> T1 = T
        ;  throw(invalid_type)
        ).
analyze(int(X), _, int, int(X)).
analyze(real(X), _, real, real(X)).
analyze(value(Name), Env, Type, Result) :-
        (  memberchk(entry(Name, Type), Env)
        -> Result = var(Name)
        ;  db:pardef(Name, Type)
        -> Result = param(Name)
        ;  throw(unknown_id)
        ).
analyze(op(O, E1, E2), Env, T, op(O, E11, E22)) :-
        analyze(E1, Env, T1, E11),
        analyze(E2, Env, T2, E22),
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


:- begin_tests(sem).

test(add) :-
        analyze(op('+', real(10.0), real(7.0)),
                op('+', real(10.0), real(7.0))).
        
test(if) :-
        analyze(if(op('<', int(1), int(2)),
                   op('+', real(10.0), real(7.0)),
                   real(5.0)),
                if(op('<', int(1), int(2)),
                   op('+', real(10.0), real(7.0)),
                   real(5.0))).

:- end_tests(sem).