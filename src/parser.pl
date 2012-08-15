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

*/

parse(Tokens, AST) :-
        phrase(definition(AST), Tokens).

definition(P) -->
        expression(P),
        expect(eof).
        
expression(E) -->
        [token(_, let, _)],
        !,
        [token(_, id, Name), token(_, '=', _)],
        expression(V),
        [token(_, in, _)],        
        expression(Z),
        { E = let(Name, V, Z) }.        
expression(E) -->
        [token(_, if, _)],
        !,
        expression(R),
        expect(then),
        expression(TL),
        expect(else),
        expression(EL),
        expect(endif),
        { E = if(R, TL, EL) }.
expression(E) -->
        relation(E),
        !.
expression(_) -->
        [token(Pos, _, _)],
        { throw(parse_error(Pos)) }.

relation(R) -->
        simple_expression(E),
        relation_aux(E, R).

relation_aux(E0, R) -->
        relational_operator(O),
        !,
        simple_expression(E),
        { R = op(O, E0, E) }.
relation_aux(E, E) -->
        [].

simple_expression(E) -->
        term(T),
        simple_expression_aux(T, E).

simple_expression_aux(E0, E) -->
        binary_adding_operator(O),
        !,
        term(T),
        simple_expression_aux(op(O, E0, T), E).
simple_expression_aux(E, E) -->
        [].

term(T) -->
        primary(F),
        term_aux(F, T).

term_aux(T0, T) -->
        multiplying_operator(O),
        !,
        primary(F),
        term_aux(op(O, T0, F), T).
term_aux(T, T) -->
        [].

primary(int(N)) -->
        [token(_, int, N)],
        !.
primary(real(N)) -->
        [token(_, real, N)],
        !.
primary(value(Name)) -->
        [token(_, id, Name)],
        !.
primary(E) -->
        [token(_, '(', _)],
        !,
        expression(E),
        expect(')').

relational_operator('<') --> [token(_, '<', _)].
relational_operator('>') --> [token(_, '>', _)].
relational_operator('=') --> [token(_, '=', _)].

binary_adding_operator('+') --> [token(_, '+', _)].
binary_adding_operator('-') --> [token(_, '-', _)].

multiplying_operator('*') --> [token(_, '*', _)].
multiplying_operator('/') --> [token(_, '/', _)].

expect(E) -->
        [token(_, E, _)],
        !.
expect(_) -->
        [token(Pos, _, _)],
        { throw(parse_error(Pos)) }.


:- begin_tests(parser).

test(basic) :-
        parse([token(_, int, 1), token(_, eof, eof)], int(1)).

test(param) :-
        parse([token(_, id, 'A001'), token(_, eof, eof)], value('A001')).

test(let) :-
        parse([token(_, let, _), token(_, id, 'a'), token(_, '=', _),
               token(_, int, 1), token(_, '+', _), token(_, int, 2),
               token(_, in, _),  token(_, id, 'a'),
               token(_, eof, eof)],
              let('a', op('+', int(1), int(2)), value('a'))).

test(if) :-
        parse([token(_, if, _),
               token(_, int, 1), token(_, '<', _), token(_, int, 2),
               token(_, then, _), token(_, int, 1),
               token(_, else, _), token(_, int, 2),
               token(_, endif, _), token(_, eof, eof)],
              if(op('<', int(1), int(2)), int(1), int(2))).

test(gt) :-
        parse([token(_, int, 1), token(_, '<', _), token(_, int, 2),
               token(_, eof, eof)],
              op('<', int(1), int(2))).

test(add) :-
        parse([token(_, int, 1), token(_, '+', _), token(_, int, 2),
               token(_, eof, eof)],
              op('+', int(1), int(2))).

test(mul) :-
        parse([token(_, int, 1), token(_, '*', _), token(_, int, 2),
               token(_, eof, eof)],
              op('*', int(1), int(2))).

test(addadd) :-
        parse([token(_, int, 1), token(_, '+', _), token(_, int, 2),
               token(_, '+', _), token(_, int, 3), token(_, eof, eof)],
              op('+', op('+', int(1), int(2)), int(3))).

test(addmul) :-
        parse([token(_, int, 1), token(_, '+', _), token(_, int, 2),
               token(_, '*', _), token(_, int, 3), token(_, eof, eof)],
              op('+', int(1), op('*', int(2), int(3)))).

:- end_tests(parser).