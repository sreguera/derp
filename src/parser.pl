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
        
expression(let(Name, Value, Body)) -->
        [token(_, let)],
        !,
        expect(id(Name)),
        expect('='),
        expression(Value),
        expect(in),        
        expression(Body).
expression(if(Condition, Then, Else)) -->
        [token(_, if)],
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
        { throw(parse_error(Pos)) }.

relation(Rel_Exp) -->
        simple_expression(Exp),
        relation_aux(Exp, Rel_Exp).

relation_aux(E0, op(Rel_Op, E0, E)) -->
        relational_operator(Rel_Op),
        !,
        simple_expression(E).
relation_aux(E, E) -->
        [].

simple_expression(E) -->
        term(T),
        simple_expression_aux(T, E).

simple_expression_aux(E0, E) -->
        binary_adding_operator(Add_Op),
        !,
        term(T),
        simple_expression_aux(op(Add_Op, E0, T), E).
simple_expression_aux(E, E) -->
        [].

term(T) -->
        primary(F),
        term_aux(F, T).

term_aux(T0, T) -->
        multiplying_operator(Mul_Op),
        !,
        primary(F),
        term_aux(op(Mul_Op, T0, F), T).
term_aux(T, T) -->
        [].

primary(int(Value)) -->
        [token(_, int(Value))],
        !.
primary(Real_Or_Unit) -->
        [token(_, real(Value))],
        !,
        (  unit_expression(Unit)
        -> { Real_Or_Unit = unit(Value, Unit) }
        ;  { Real_Or_Unit = real(Value) }
        ).
primary(Fun_Or_Value) -->
        [token(_, id(Name))],
        !,
        (  arguments(Arg)
        -> { Fun_Or_Value = fun(Name, Arg) }
        ;  { Fun_Or_Value = value(Name) }
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
        multiplying_operator(Mul_Op),
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

relational_operator('<') --> [token(_, '<')].
relational_operator('>') --> [token(_, '>')].
relational_operator('=') --> [token(_, '=')].

binary_adding_operator('+') --> [token(_, '+')].
binary_adding_operator('-') --> [token(_, '-')].

multiplying_operator('*') --> [token(_, '*')].
multiplying_operator('/') --> [token(_, '/')].


%% expect(+Element)
%
% Match Element as the next token in the list and discards it.
% If not, throws a parser exception.

expect(Element) -->
        [token(_, Element)],
        !.
expect(_) -->
        [token(Pos, _)],
        { throw(parse_error(Pos)) }.


:- begin_tests(parser).

test(basic) :-
        parse([token(_, int(1)), token(_, eof)], int(1)).

test(param) :-
        parse([token(_, id('A001')), token(_, eof)], value('A001')).

test(let) :-
        parse([token(_, let), token(_, id('a')), token(_, '='),
               token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, in),  token(_, id('a')),
               token(_, eof)],
              let('a', op('+', int(1), int(2)), value('a'))).

test(if) :-
        parse([token(_, if),
               token(_, int(1)), token(_, '<'), token(_, int(2)),
               token(_, then), token(_, int(1)),
               token(_, else), token(_, int(2)),
               token(_, endif), token(_, eof)],
              if(op('<', int(1), int(2)), int(1), int(2))).

test(gt) :-
        parse([token(_, int(1)), token(_, '<'), token(_, int(2)),
               token(_, eof)],
              op('<', int(1), int(2))).

test(add) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, eof)],
              op('+', int(1), int(2))).

test(mul) :-
        parse([token(_, int(1)), token(_, '*'), token(_, int(2)),
               token(_, eof)],
              op('*', int(1), int(2))).

test(addadd) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, '+'), token(_, int(3)), token(_, eof)],
              op('+', op('+', int(1), int(2)), int(3))).

test(addmul) :-
        parse([token(_, int(1)), token(_, '+'), token(_, int(2)),
               token(_, '*'), token(_, int(3)), token(_, eof)],
              op('+', int(1), op('*', int(2), int(3)))).

:- end_tests(parser).