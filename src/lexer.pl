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
:- module(lexer, [scan/2]).


scan([], []).
scan([Char|Chars], Tokens) :-
        separator(Char),
        !,
        scan(Chars, Tokens).
scan([Char|Chars], [Token|Tokens]) :-
        delimiter(Char),
        !,
        atom_codes(Token, [Char]),
        scan(Chars, Tokens).
scan([Char|Chars], [Token|Tokens]) :-
        digit_char(Char),
        !,
        span(digit_char, Chars, Extend, RestChars),
        number_codes(Number, [Char|Extend]),
        Token = num(Number),
        scan(RestChars, Tokens).


separator(0'\t).
separator(0'\n).
separator(0'\v).
separator(0'\f).
separator(0'\r).
separator(0'\ ).


delimiter(0'*).
delimiter(0'+).
delimiter(0'<).
delimiter(0';).


digit_char(C) :-
        code_type(C, digit).


%% span(+Pred, +List, -Prefix, -Remainder)
% Prefix is the longest prefix of elements of List that satisfies Pred
% Remainder is the remainder of the list
span(Pred, [Elem|List], [Elem|Prefix], Remainder) :-
        call(Pred, Elem),
        !,
        span(Pred, List, Prefix, Remainder).
span(_, Remainder, [], Remainder).


:- begin_tests(lexer).

test(delimiters) :-
        scan("+*<", ['+', '*', '<']).

test(numbers) :-
        scan("123 234",
             [num(123), num(234)]).

:- end_tests(lexer).