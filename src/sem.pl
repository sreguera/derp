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

  type expression ---> let(name :: atom,
                           value :: expression,
                           body :: expression)
                     ; if(condition :: expression,
                          then :: expression,
                          else :: expression)
                     ; op(operator,
                          left :: expression,
                          right :: expression)
                     ; fun(name :: fname,
                           argument :: expression)
                     ; int(value :: integer)
                     ; real(value :: float)
                     ; var(name :: atom)
                     ; param(name :: atom)

  type operator ---> imul ; rmul
                   ; idiv ; rdiv
                   ; iadd ; radd
                   ; isub ; rsub
                   ; ilt ; rlt
                   ; igt ; rgt
                   ; ieq ; req

  type fname ---> sin ; cos ; tan

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
analyze(op(O, E1, E2), Env, T, op(OO, E11, E22)) :-
        analyze(E1, Env, T1, E11),
        analyze(E2, Env, T2, E22),
        (  op(O, T1, T2, TR, Op)
        -> T = TR, OO = Op
        ;  throw(invalid_op)
        ).
analyze(fun(Name, E), Env, T, fun(Name, E1)) :-
        analyze(E, Env, T1, E1),
        (  fun(Name, T1, T)
        -> !
        ;  throw(invalid_op)
        ).

fun('sin', real, real).
fun('cos', real, real).
fun('tan', real, real).

op('+', int, int, int, iadd).
op('+', real, real, real, radd).
op('-', int, int, int, isub).
op('-', real, real, real, rsub).
op('*', int, int, int, imul).
op('*', real, real, real, rmul).
op('/', int, int, int, idiv).
op('/', real, real, real, rdiv).
op('<', int, int, bool, ilt).
op('<', real, real, bool, rlt).
op('>', int, int, bool, igt).
op('>', real, real, bool, rgt).
op('=', int, int, bool, ieq).
op('=', real, real, bool, req).


:- begin_tests(sem).

test(add) :-
        analyze(op('+', real(10.0), real(7.0)),
                op(radd, real(10.0), real(7.0))).
        
test(if) :-
        analyze(if(op('<', int(1), int(2)),
                   op('+', real(10.0), real(7.0)),
                   real(5.0)),
                if(op(ilt, int(1), int(2)),
                   op(radd, real(10.0), real(7.0)),
                   real(5.0))).

:- end_tests(sem).