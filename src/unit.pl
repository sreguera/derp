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

This module provides operations for handling units of measure.

  type unit ---> [factor]

  type factor ---> unit :: atom - exponent :: integer

*/


%% canon(+TreeUnit, -CanonUnit)

canon(op(*, L, R), C) :-
        canon(L, LC),
        canon(R, RC),
        prod(LC, RC, C).
canon(op(/, L, R), C) :-
        canon(L, LC),
        canon(R, RC),
        div(LC, RC, C).
canon(op(^, U, E), [U-E]).


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


%% neg_exp(+Factor, -InverseFactor) 

inv_factor(U-E, U-E_Inverted) :-
        E_Inverted is E * -1.


%% sum_exp(+MultiFactor, -Factor)

sum_exp(U-E_List, U-E) :-
        sumlist(E_List, E).
