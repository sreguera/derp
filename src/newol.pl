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
:- module(newol, []).

:- use_module(lexer).
:- use_module(parser).
:- use_module(sem).
:- use_module(interp).

:- multifile prolog_message//1.

prolog:message(unexpected_char(Char, pos(L, C))) -->
        ['Unexpected char "~c" at line ~d, column ~d'-[Char, L, C]].
prolog:message(parse_error(pos(L, C))) -->
        ['Parse error at line ~d, column ~d'-[L, C]].

execute_source(Input, Output) :-
        lexer:scan(Input, Tokens),
        parser:parse(Tokens, AST),
        sem:analyze(AST, AST2),
        interp:execute(AST2, Output).

execute_file(Input_File) :-
        read_file_to_codes(Input_File, Source, []),
        execute_source(Source, Output),
        print(Output), nl.

execute_with_args(_Opts, PosArgs) :-
        PosArgs = [Input_File],
        execute_file(Input_File).

run :-
        current_prolog_flag(argv, Args),
        append(_SysArgs, ['--'|AppArgs], Args),
        !,
        parse_args(AppArgs, Opts, PosArgs),
        catch(execute_with_args(Opts, PosArgs),
              E,
              (print_message(error, E), fail)).


%% parse_args(+Args, -Options, -PositionalArgs)

parse_args([], [], []).
parse_args([Arg|Args], Opts, [Arg|PosArgs]) :-
        parse_args(Args, Opts, PosArgs).

:- begin_tests(newol).

test(lexer_error, [throws(unexpected_char(0'!, _))]) :-
        execute_source("!", _).

test(parser_error, [throws(parse_error(_))]) :-
        execute_source("if if", _).

test(invalid_cond, [throws(invalid_cond)]) :-
        execute_source("if 1 then 1 else 2 endif", _).

test(invalid_type, [throws(invalid_type)]) :-
        execute_source("if 1 < 2 then 1 else 1.0 endif", _).

test(unknown_id, [throws(unknown_id)]) :-
        execute_source("foo", _).

test(invalid_op, [throws(invalid_op)]) :-
        execute_source("1.0 + 1", _).

test(int) :-
        execute_source("5", 5),
        execute_source("4+5", 9),
        execute_source("4*5", 20).

test(real) :-
        execute_source("5.0", 5.0),
        execute_source("4.0+5.0", 9.0),
        execute_source("4.0*5.0", 20.0).

test(comp) :-
        execute_source("1 > 2", false),
        execute_source("1 < 2", true).
        
test(fun) :-
        execute_source("sin(0.0)", 0.0),
        execute_source("cos(0.0)", 1.0).

test(if) :-
        execute_source("if 1 < 2 then 3 else 4 endif", 3).

:- end_tests(newol).