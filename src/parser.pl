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


parse(Tokens, AST) :-
        phrase(expression_list(AST), Tokens).

expression_list(L) -->
        expression(E),
        expression_list_aux(E, L).

expression_list_aux(E0, L) -->
        [token(_, ';', _)],
        !,
        expression(E),
        expression_list_aux(seq(E0, E), L).
expression_list_aux(E, E) -->
        [].

expression(E) -->
        [token(_, if, _)],
        !,
        relation(R),
        [token(_, then, _)],
        expression_list(TL),
        [token(_, else, _)],
        expression_list(EL),
        [token(_, endif, _)],
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

primary(num(N)) -->
        [token(_, num, N)].

relational_operator('<') --> [token(_, '<', _)].

binary_adding_operator('+') --> [token(_, '+', _)].

multiplying_operator('*') --> [token(_, '*', _)].

:- begin_tests(parser).

test(basic) :-
        parse([token(_, num, 1)], num(1)).

test(if) :-
        parse([token(_, if, _),
               token(_, num, 1), token(_, '<', _), token(_, num, 2),
               token(_, then, _), token(_, num, 1),
               token(_, else, _), token(_, num, 2),
               token(_, endif, _)],
              if(op('<', num(1), num(2)), num(1), num(2))).

test(seq) :-
        parse([token(_, num, 1), token(_, ';', _), token(_, num, 2)],
              seq(num(1), num(2))).

test(gt) :-
        parse([token(_, num, 1), token(_, '<', _), token(_, num, 2)],
              op('<', num(1), num(2))).

test(add) :-
        parse([token(_, num, 1), token(_, '+', _), token(_, num, 2)],
              op('+', num(1), num(2))).

test(mul) :-
        parse([token(_, num, 1), token(_, '*', _), token(_, num, 2)],
              op('*', num(1), num(2))).

test(addadd) :-
        parse([token(_, num, 1), token(_, '+', _), token(_, num, 2),
               token(_, '+', _), token(_, num, 3)],
              op('+', op('+', num(1), num(2)), num(3))).

test(addmul) :-
        parse([token(_, num, 1), token(_, '+', _), token(_, num, 2),
               token(_, '*', _), token(_, num, 3)],
              op('+', num(1), op('*', num(2), num(3)))).

:- end_tests(parser).