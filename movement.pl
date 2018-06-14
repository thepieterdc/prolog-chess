:- module(movement, []).

:- use_module(board).
:- use_module(pawn, [move//1 as pawn_move]).

best(State, Move) :- phrase(pawn_move(State), Moves), nth0(0, Moves, Move).

forward(square(SR, C), black, square(SR1, C)) :- SR1 is SR - 1, SR1 >= 1.
forward(square(SR, C), white, square(SR1, C)) :- SR1 is SR + 1, SR1 =< 8.

% checkt niet de square zelf want bij bvb capture kan dit de bedoeling zijn dat die niet free is.
path_forward_clear(Board, From, Color, To) :-
  forward(From, Color, Next),
  path_forward_clear_sub(Board, Next, Color, To).

% subroutine zodat eerste square niet leeg moet zijn want dat zou achterlijk zijn
path_forward_clear_sub(_, Square, _, Square).
path_forward_clear_sub(Board, From, Color, To) :-
  board:free(Board, From),
  forward(From, Color, Next),
  path_forward_clear(Board, Next, Color, To).

pawn_forward(From, black, To) :- forward(From, black, To).
pawn_forward(square(7, C), black, square(5, C)) :- between(1, 8, C).
pawn_forward(From, white, To) :- forward(From, white, To).
pawn_forward(square(2, C), white, square(4, C)) :- between(1, 8 ,C).
