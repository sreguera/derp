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
        initial_env(Env),
        analyze(AST, Env, _, AST2).


%% analyze(+Expression, +Environment, -Type, -NewExpression)

analyze(let(_Pos, Name, Value0, Body0), Env0, Body_Type,
        let(Name, Value, Body)) :-
        analyze(Value0, Env0, Value_Type, Value),
        put_env(Name, Env0, Value_Type, Env),
        analyze(Body0, Env, Body_Type, Body).
analyze(if(Pos, Condition0, Then0, Else0), Env, If_Type,
        if(Condition, Then, Else)) :-
        analyze(Condition0, Env, Condition_Type, Condition),
        (  Condition_Type = bool, !
        ;  throw(incompatible_if_cond(Pos, Condition_Type))
        ),
        analyze(Then0, Env, Then_Type, Then),
        analyze(Else0, Env, Else_Type, Else),
        (  Then_Type = Else_Type
        -> Then_Type = If_Type
        ;  throw(incompatible_if_types(Pos, Then_Type, Else_Type))
        ).
analyze(int(_Pos, Value), _Env, int, int(Value)).
analyze(real(_Pos, Value), _Env, real, real(Value)).
analyze(unit(_Pos, Value, Unit), _Env, unit(Unit), real(Value)). 
analyze(value(Pos, Name), Env, Type, Var_Or_Param) :-
        (  get_env(Name, Env, Type)
        -> Var_Or_Param = var(Name)
        ;  db:pardef(Name, Type)
        -> Var_Or_Param = param(Name)
        ;  throw(unknown_id(Pos, Name))
        ).
analyze(op(Pos, Op0, Left0, Right0), Env, Op_Type, op(Op, Left, Right)) :-
        analyze(Left0, Env, Left_Type, Left),
        analyze(Right0, Env, Right_Type, Right),
        (  op(Op0, Left_Type, Right_Type, Op_Type, Op), !
        ;  throw(invalid_op(Pos, Op0, Left_Type, Right_Type))
        ).
analyze(fun(_Pos, Name, Arg0), Env, Fun_Type, fun(Name, Arg)) :-
        analyze(Arg0, Env, Arg_Type, Arg),
        (  fun(Name, Arg_Type, Fun_Type), !
        ;  throw(invalid_op)
        ).


put_env(Name, Env, Type, [entry(Name, Type)|Env]).

get_env(Name, Env, Type) :-
        memberchk(entry(Name, Type), Env).

initial_env(Env) :-
        setof(entry(Name, Type), var(Name, Type), Env).


%% var(Name, Type)

var('true', bool).
var('false', bool).
var('pi', real).
var('e', real).


%% fun(Name, Arg_Type, Result_Type)

fun('sin', real, real).
fun('cos', real, real).
fun('tan', real, real).


%% op(Name, Left_Type, Right_Type, Result_Type, Typed_Op)

op('+', int, int, int, iadd).
op('+', real, real, real, radd).
op('+', unit(U), unit(U), unit(U), radd).
op('-', int, int, int, isub).
op('-', real, real, real, rsub).
op('-', unit(U), unit(U), unit(U), rsub).
op('*', int, int, int, imul).
op('*', real, real, real, rmul).
op('*', real, unit(U), unit(U), rmul).
op('*', unit(U), real, unit(U), rmul).
op('*', unit(U1), unit(U2), unit(U), rmul) :-
        U = op('*', U1, U2).
op('/', int, int, int, idiv).
op('/', real, real, real, rdiv).
op('/', real, unit(U), unit(U), rdiv).
op('/', unit(U), real, unit(U), rdiv).
op('/', unit(U1), unit(U2), unit(U), rdiv) :-
        U = op('/', U1, U2).
op('<', int, int, bool, ilt).
op('<', real, real, bool, rlt).
op('<', unit(U), unit(U), bool, rlt).
op('>', int, int, bool, igt).
op('>', real, real, bool, rgt).
op('>', unit(U), unit(U), bool, rgt).
op('=', int, int, bool, ieq).
op('=', real, real, bool, req).
op('=', unit(U), unit(U), bool, req).


:- begin_tests(sem).

test(add) :-
        analyze(op(_, '+', real(_, 10.0), real(_, 7.0)),
                op(radd, real(10.0), real(7.0))).
        
test(if) :-
        analyze(if(_, op(_, '<', int(_, 1), int(_, 2)),
                   op(_, '+', real(_, 10.0), real(_, 7.0)),
                   real(_, 5.0)),
                if(op(ilt, int(1), int(2)),
                   op(radd, real(10.0), real(7.0)),
                   real(5.0))).

:- end_tests(sem).