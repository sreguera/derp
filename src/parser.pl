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

:- module(parser, [parse/2]).

/** <module> Parser

This module provides operations for converting a list of tokens to an
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
                     ; fun(name :: atom,
                           argument :: expression)
                     ; int(value :: integer)
                     ; real(value :: float)
                     ; unit(value :: float, unit)
                     ; value(name :: atom)

  type operator ---> * ; / ; + ; - ; < ; > ; =  

  type unit ---> op(mult_operator,
                    left :: unit,
                    right :: unit)
               ; op(^,
                    id(name :: atom),
                    power :: integer)
               ; id(name :: atom)
  
  type mult_operator ---> * ; /
  
  
*/

parse(Tokens, AST) :-
        phrase(full_expression(AST), Tokens).

full_expression(P) -->
        expression(P),
        expect(eof).
        
expression(let(Pos, Name, Value, Body)) -->
        [token(Pos, let)],
        !,
        expect(id(Name)),
        expect('='),
        expression(Value),
        expect(in),        
        expression(Body).
expression(if(Pos, Condition, Then, Else)) -->
        [token(Pos, if)],
        !,
        expression(Condition),
        expect(then),
        expression(Then),
        expect(else),
        expression(Else),
        expect(endif).
expression(E) -->
        relation(E),
        !.
expression(_) -->
        [token(Pos, _)],
        { throw(unexpected_syntax(Pos, expression)) }.

relation(Rel_Exp) -->
        simple_expression(Exp),
        relation_aux(Exp, Rel_Exp).

relation_aux(E0, op(Pos, Rel_Op, E0, E)) -->
        relational_operator(Rel_Op, Pos),
        !,
        simple_expression(E).
relation_aux(E, E) -->
        [].

simple_expression(E) -->
        term(T),
        simple_expression_aux(T, E).

simple_expression_aux(E0, E) -->
        binary_adding_operator(Add_Op, Pos),
        !,
        term(T),
        simple_expression_aux(op(Pos, Add_Op, E0, T), E).
simple_expression_aux(E, E) -->
        [].

term(T) -->
        primary(F),
        term_aux(F, T).

term_aux(T0, T) -->
        multiplying_operator(Mul_Op, Pos),
        !,
        primary(F),
        term_aux(op(Pos, Mul_Op, T0, F), T).
term_aux(T, T) -->
        [].

primary(int(Pos, Value)) -->
        [token(Pos, int(Value))],
        !.
primary(Real_Or_Unit) -->
        [token(Pos, real(Value))],
        !,
        (  unit_expression(Unit)
        -> { Real_Or_Unit = unit(Pos, Value, Unit) }
        ;  { Real_Or_Unit = real(Pos, Value) }
        ).
primary(Fun_Or_Value) -->
        [token(Pos, id(Name))],
        !,
        (  arguments(Arg)
        -> { Fun_Or_Value = fun(Pos, Name, Arg) }
        ;  { Fun_Or_Value = value(Pos, Name) }
        ).
primary(Paren_Exp) -->
        [token(_, '(')],
        !,
        expression(Paren_Exp),
        expect(')').

arguments(Arg) -->
        [token(_, '(')],
        expression(Arg),
        expect(')').


% Units

unit_expression(Unit) -->
        [token(_, '\'')],
        unit_term(Unit),
        expect('\'').

unit_term(T) -->
        unit_factor(F),
        unit_term_aux(F, T).

unit_term_aux(T0, T) -->
        multiplying_operator(Mul_Op, _Pos),
        !,
        unit_factor(F),
        unit_term_aux(op(Mul_Op, T0, F), T).
unit_term_aux(T, T) -->
        [].

unit_factor(Base_Or_Power) -->
        [token(_, id(Base))],
        !,
        (  [token(_, '^'), token(_, int(Power))]
        -> { Base_Or_Power = op('^', id(Base), Power) }
        ;  { Base_Or_Power = id(Base) }
        ).
unit_factor(Paren_Unit) -->
        [token(_, '(')],
        !,
        unit_term(Paren_Unit),
        expect(')').


% Operators

relational_operator('<', Pos) --> [token(Pos, '<')].
relational_operator('>', Pos) --> [token(Pos, '>')].
relational_operator('=', Pos) --> [token(Pos, '=')].

binary_adding_operator('+', Pos) --> [token(Pos, '+')].
binary_adding_operator('-', Pos) --> [token(Pos, '-')].

multiplying_operator('*', Pos) --> [token(Pos, '*')].
multiplying_operator('/', Pos) --> [token(Pos, '/')].


%% expect(+Element)
%
% Match Element as the next token in the list and discards it.
% If not, throws a parser exception.

expect(Element) -->
        [token(_, Element)],
        !.
expect(Expected) -->
        [token(Pos, Got)],
        { throw(unexpected_token(Pos, Got, Expected)) }.


:- begin_tests(parser).

test(basic) :-
        parse([token(Pos, int(1)), token(_, eof)], int(Pos, 1)).

test(param) :-
        parse([token(Pos, id('A001')), token(_, eof)], value(Pos, 'A001')).

test(let) :-
        parse([token(_, let), token(_, id('a')), token(_, '='),
               token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, in),  token(_, id('a')),
               token(_, eof)],
              let(_, 'a', op(_, '+', int(_, 1), int(_, 2)), value(_, 'a'))).

test(if) :-
        parse([token(_, if),
               token(_, int(1)), token(_, '<'), token(_, int(2)),
               token(_, then), token(_, int(1)),
               token(_, else), token(_, int(2)),
               token(_, endif), token(_, eof)],
              if(_, op(_, '<', int(_, 1), int(_, 2)), int(_, 1), int(_, 2))).

test(gt) :-
        parse([token(_, int(1)), token(_, '<'), token(_, int(2)),
               token(_, eof)],
              op(_, '<', int(_, 1), int(_, 2))).

test(add) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, eof)],
              op(_, '+', int(_, 1), int(_, 2))).

test(mul) :-
        parse([token(_, int(1)), token(_, '*'), token(_, int(2)),
               token(_, eof)],
              op(_, '*', int(_, 1), int(_, 2))).

test(addadd) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, '+'), token(_, int(3)), token(_, eof)],
              op(_, '+', op(_, '+', int(_, 1), int(_, 2)), int(_, 3))).

test(addmul) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, '*'), token(_, int(3)), token(_, eof)],
              op(_, '+', int(_, 1), op(_, '*', int(_, 2), int(_, 3)))).

:- end_tests(parser).