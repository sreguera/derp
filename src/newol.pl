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
:- use_module(interp).

:- multifile prolog_message//1.

prolog:message(unexpected_char(Char, pos(L, C))) -->
        ['Unexpected char "~c" at line ~d, column ~d'-[Char, L, C]].
prolog:message(parse_error(pos(L, C))) -->
        ['Parse error at line ~d, column ~d'-[L, C]].

execute_source(Input, Output) :-
        lexer:scan(Input, Tokens),
        parser:parse(Tokens, AST),
        interp:execute(AST, Output).

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
