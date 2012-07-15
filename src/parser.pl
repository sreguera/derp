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
        [';'],
        !,
        expression(E),
        expression_list_aux(seq(E0, E), L).
expression_list_aux(E, E) -->
        [].

expression(E) -->
        [if],
        !,
        relation(R),
        [then],
        expression_list(TL),
        [else],
        expression_list(EL),
        [endif],
        { E = if(R, TL, EL) }.
expression(E) -->
        relation(E),
        !.
expression(_) -->
        { throw(parse_error) }.

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
        [num(N)].

relational_operator('<') --> ['<'].

binary_adding_operator('+') --> ['+'].

multiplying_operator('*') --> ['*'].

:- begin_tests(parser).

test(basic) :-
        parse([num(1)], num(1)).

test(if) :-
        parse([if, num(1), '<', num(2), then, num(1), else, num(2), endif],
              if(op('<', num(1), num(2)), num(1), num(2))).

test(seq) :-
        parse([num(1), ';', num(2)], seq(num(1), num(2))).

test(gt) :-
        parse([num(1), '<', num(2)], op('<', num(1), num(2))).

test(add) :-
        parse([num(1), '+', num(2)], op('+', num(1), num(2))).

test(mul) :-
        parse([num(1), '*', num(2)], op('*', num(1), num(2))).

test(addadd) :-
        parse([num(1), '+', num(2), '+', num(3)],
              op('+', op('+', num(1), num(2)), num(3))).

test(addmul) :-
        parse([num(1), '+', num(2), '*', num(3)],
              op('+', num(1), op('*', num(2), num(3)))).

:- end_tests(parser).