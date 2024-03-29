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

:- module(unit, []).

/** <module> Unit handler

This module provides operations for handling units of measurement.

  type unit ---> [factor]

  type factor ---> unit :: atom - exponent :: integer

*/


%% canon(+Tree_Unit, -Canon_Unit, -Power_Of_Ten)

canon(op(O, L, R), C, P) :-
        canon(O, L, R, C, P).

canon(*, L, R, C, P) :-
        canon(L, LC, LP),
        canon(R, RC, RP),
        prod(LC, RC, C),
        P is LP + RP.
canon(/, L, R, C, P) :-
        canon(L, LC, LP),
        canon(R, RC, RP),
        div(LC, RC, C),
        P is LP - RP.
canon(^, U, E, Res, PE) :-
        canon1(U, B, P),
        maplist(pow_factor(E), B, Res),
        PE is P * E.


%% prod(+Left, +Right, -Result)

prod(L, R, X) :-
        append(L, R, LR),
        keysort(LR, LR_Sorted),
        group_pairs_by_key(LR_Sorted, LR_Grouped),
        maplist(sum_exp, LR_Grouped, X).


%% prod(+Left, +Right, -Result)

div(L, R, X) :-
        maplist(inv_factor, R, R_Inverted),
        prod(L, R_Inverted, X).        


%% inv_factor(+Factor, -Inverse_Factor) 

inv_factor(U-E, U-E_Inverted) :-
        E_Inverted is E * -1.


%% sum_exp(+Multi_Factor, -Factor)

sum_exp(U-E_List, U-E) :-
        sumlist(E_List, E).


%% pow_factor(+Exp, +Factor, -Powered_Factor)

pow_factor(E, U-Ue, U-UeE) :-
        UeE is Ue * E.


%% unit(Symbol, Base_Expression)

unit('m', ['m'-1]). % metre
unit('s', ['s'-1]). % second
unit('g', ['g'-1]). % gram
unit('A', ['A'-1]). % Ampere
unit('K', ['K'-1]). % Kelvin
unit('N', ['kg'-1, 'm'-1, 's'- -2]). % Newton
unit('Pa', ['kg'-1, 'm'- -1, 's'- -2]). % Pascal

%% prefix(Symbol, Power_Of_Ten)

prefix('G', 9).
prefix('M', 6).
prefix('k', 3).
prefix('h', 2).
prefix('da', 1).
prefix('d', -1).
prefix('c', -2).
prefix('m', -3).
prefix('u', -6).
prefix('n', -9).


canon1(Unit, Base_Unit, Power_Of_Ten) :-
        (  Unit = 'g'
        -> Base_Unit = ['kg'-1],
           Power_Of_Ten = -3
        ;  Unit = 'kg'
        -> Base_Unit = ['kg'-1],
           Power_Of_Ten = 0
        ;  unit(Unit, Base_Unit)
        -> Power_Of_Ten = 0
        ;  prefix(Prefix, Power_Of_Ten),
           unit(Unit_Name, Base_Unit),
           atom_concat(Prefix, Unit_Name, Unit)
        -> true
        ).


:- begin_tests(unit).

test(simple) :-
        canon(op(/, op(^, 'm', 1), op(^, 's', 2)),
              ['m'-1, 's'- -2],
              0).

test(simple2) :-
        canon(op(/, op(^, 'm', 1), op(^, 'ms', 2)),
              ['m'-1, 's'- -2],
              6).

:- end_tests(unit).