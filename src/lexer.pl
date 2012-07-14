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

scan(Chars, Tokens) :-
        scan(Chars, pos(1, 1), Tokens).

scan([], _, []).
scan([Char|Chars], Pos, Tokens) :-
        separator(Char),
        !,
        update_pos([Char], Pos, Pos1),
        scan(Chars, Pos1, Tokens).
scan([Char|Chars], Pos, [Token|Tokens]) :-
        delimiter(Char),
        !,
        atom_codes(Token, [Char]),
        update_pos([Char], Pos, Pos1),
        scan(Chars, Pos1, Tokens).
scan([Char|Chars], Pos, [Token|Tokens]) :-
        digit_char(Char),
        !,
        span(digit_char, Chars, Extend, RestChars),
        number_codes(Number, [Char|Extend]),
        Token = num(Number),
        update_pos([Char|Extend], Pos, Pos1),
        scan(RestChars, Pos1, Tokens).
scan([Char|_], Pos, _) :-
        throw(unexpected_char(Char, Pos)).

end_of_line(0'\n). 

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


update_pos([], pos(L, C), pos(L, C)).
update_pos([X], pos(L0, _), pos(L, C)) :-
        end_of_line(X),
        !,
        L is L0 + 1, C is 1.
update_pos([_|Xs], pos(L, C0), pos(L, C)) :-
        length(Xs, Len),
        C is C0 + Len + 1.

%% span(+Pred, +List, -Prefix, -Remainder)
% Prefix is the longest prefix of elements of List that satisfies Pred
% Remainder is the remainder of the list
span(Pred, [Elem|List], [Elem|Prefix], Remainder) :-
        call(Pred, Elem),
        !,
        span(Pred, List, Prefix, Remainder).
span(_, Remainder, [], Remainder).


:- begin_tests(lexer).

test(pos) :-
        update_pos("hello", pos(1, 1), pos(1, 6)).

test(delimiters) :-
        scan("+*<", ['+', '*', '<']).

test(numbers) :-
        scan("123 234", [num(123), num(234)]).

test(error, [throws(unexpected_char(0'!, pos(2, 5)))]) :-
        scan("\n123 !", _).

:- end_tests(lexer).