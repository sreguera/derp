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
:- module(sem, [analyze/2]).


analyze(seq(E1, E2), T) :-
        analyze(E1, _),
        analyze(E2, T).
analyze(if(C, E1, E2), T) :-
        analyze(C, CT),
        (  CT = bool
        ;  throw(invalid_cond)
        ),
        analyze(E1, T1),
        analyze(E2, T2),
        (  T1 = T2
        -> T1 = T
        ;  throw(invalid_type)
        ).
analyze(num(_), num).
analyze(op(O, E1, E2), T) :-
        analyze(E1, T1),
        analyze(E2, T2),
        (  op(O, T1, T2, TR)
        -> T = TR
        ;  throw(invalid_op)
        ).

op('+', num, num, num).
op('*', num, num, num).
op('<', num, num, bool).
