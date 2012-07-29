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

scan([], Pos, [token(Pos, eof, eof)]).
scan([Char|Chars], Pos, Tokens) :-
        (  separator(Char)
        -> scan_separator(Char, Chars, Pos, Tokens)
        ;  delimiter(Char)
        -> scan_delimiter(Char, Chars, Pos, Tokens)
        ;  identifier_start_char(Char)
        -> scan_id(Char, Chars, Pos, Tokens)
        ;  digit_char(Char)
        -> scan_number(Char, Chars, Pos, Tokens)
        ;  throw(unexpected_char(Char, Pos))
        ).

scan_separator(Char, Chars, Pos, Tokens) :-
        update_pos([Char], Pos, Pos1),
        scan(Chars, Pos1, Tokens).

scan_delimiter(Char, Chars, Pos, [token(Pos, Type, Type)|Tokens]) :-
        atom_codes(Type, [Char]),
        update_pos([Char], Pos, Pos1),
        scan(Chars, Pos1, Tokens).

scan_id(Char, Chars, Pos, [token(Pos, Type, Value)|Tokens]) :-
        span(identifier_extend_char, Chars, Extend, RestChars),
        atom_codes(Identifier, [Char|Extend]),
        (  reserved_word(Identifier)
        -> Type = Identifier, Value = Identifier
        ;  Type = id, Value = Identifier
        ),
        update_pos([Char|Extend], Pos, Pos1),
        scan(RestChars, Pos1, Tokens).

scan_number(Char, Chars, Pos, Tokens) :-
        span(digit_char, Chars, Extend, RestChars),
        scan_number_aux(RestChars, [Char|Extend], Pos, Tokens).

scan_number_aux([], NumChars, Pos, [token(Pos, int, Number)|Tokens]) :-
        number_codes(Number, NumChars),
        update_pos(NumChars, Pos, Pos1),
        scan([], Pos1, Tokens).
scan_number_aux([Char|Chars], NumChars, Pos, [Token|Tokens]) :-
        (  Char = 0'.
        -> span(digit_char, Chars, Extend, RestChars),
           append([NumChars, [Char], Extend], RealChars), 
           number_codes(Real, RealChars),
           update_pos(RealChars, Pos, Pos1),
           Token = token(Pos, real, Real),
           scan(RestChars, Pos1, Tokens)
        ;  number_codes(Number, NumChars),
           update_pos(NumChars, Pos, Pos1),
           Token = token(Pos, int, Number),
           scan([Char|Chars], Pos1, Tokens)
        ).


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
delimiter(0'().
delimiter(0')).
delimiter(0'=).

identifier_start_char(C) :-
        code_type(C, alpha).

identifier_extend_char(C) :-
        code_type(C, alnum).

digit_char(C) :-
        code_type(C, digit).


reserved_word(if).
reserved_word(then).
reserved_word(else).
reserved_word(endif).
reserved_word(let).


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
        scan("+*<",
             [token(_, '+', '+'), token(_, '*', '*'), token(_, '<', '<'),
              token(_, eof, eof)]).

test(keywords) :-
        scan("if then else endif",
             [token(_, if, if), token(_, then, then),
              token(_, else, else), token(_, endif, endif),
              token(_, eof, eof)]).

test(numbers) :-
        scan("123 2.5 234",
             [token(_, int, 123), token(_, real, 2.5), token(_, int, 234),
              token(_, eof, eof)]).

test(error, [throws(unexpected_char(0'!, pos(2, 5)))]) :-
        scan("\n123 !", _).

:- end_tests(lexer).