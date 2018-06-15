:- module(movement, []).

:- use_module(board).
:- use_module(pawn, [move//1 as pawn_move]).
:- use_module(rook, [move//1 as rook_move]).

best(State, Move) :-
  phrase(move(State), Moves), nth0(0, Moves, Move).

move(State) --> pawn_move(State).
move(State) --> rook_move(State).

% checkt niet de square zelf want bij bvb capture kan dit de bedoeling zijn dat die niet free is.
path_clear(Board, From, Color, Direction, To) :-
  position(From, Color, Direction, Next),
  path_clear_sub(Board, Next, Color, Direction, To).

% subroutine zodat eerste square niet leeg moet zijn want dat zou achterlijk zijn
path_clear_sub(_, Square, _, _, Square).
path_clear_sub(Board, From, Color, Direction, To) :-
  board:free(Board, From),
  position(From, Color, Direction, Next),
  path_clear_sub(Board, Next, Color, Direction, To).

pawn_capture(From, Color, To) :- position(From, Color, forward_left, To).
pawn_capture(From, Color, To) :- position(From, Color, forward_right, To).

pawn_forward(From, black, To) :- position(From, black, forward, To).
pawn_forward(square(7, C), black, square(5, C)) :- between(1, 8, C).
pawn_forward(From, white, To) :- position(From, white, forward, To).
pawn_forward(square(2, C), white, square(4, C)) :- between(1, 8 ,C).

position(square(R, C), black, backward, square(R1, C)) :- R1 is R - 1, R1 >= 1.
position(square(R, C), white, backward, square(R1, C)) :- R1 is R + 1, R1 =< 8.
position(square(R, C), black, forward, square(R1, C)) :- R1 is R - 1, R1 >= 1.
position(square(R, C), white, forward, square(R1, C)) :- R1 is R + 1, R1 =< 8.
position(square(R, C), black, forward_left, square(R1, C1)) :- R1 is R - 1, C1 is C + 1, R1 >= 1, C1 =< 8.
position(square(R, C), white, forward_left, square(R1, C1)) :- R1 is R + 1, C1 is C - 1, R1 =< 8, C1 >= 1.
position(square(R, C), black, forward_right, square(R1, C1)) :- R1 is R - 1, C1 is C - 1, R1 >= 1, C1 >= 1.
position(square(R, C), white, forward_right, square(R1, C1)) :- R1 is R + 1, C1 is C + 1, R1 =< 8, C1 =< 8.
position(square(R, C), black, left, square(R, C1)) :- C1 is C + 1, C1 =< 8.
position(square(R, C), white, left, square(R, C1)) :- C1 is C - 1, C1 >= 1.
position(square(R, C), black, right, square(R, C1)) :- C1 is C - 1, C1 >= 1.
position(square(R, C), white, right, square(R, C1)) :- C1 is C + 1, C1 =< 8.

rook(square(R, C), black, backward, square(R1, C)) :- between(1, 8, R1), R1 > R.
rook(square(R, C), white, backward, square(R1, C)) :- between(1, 8, R1), R1 < R.
rook(square(R, C), black, forward, square(R1, C)) :- between(1, 8, R1), R > R1.
rook(square(R, C), white, forward, square(R1, C)) :- between(1, 8, R1), R < R1.
rook(square(R, C), black, left, square(R, C1)) :- between(1, 8, C1), C1 > C.
rook(square(R, C), white, left, square(R, C1)) :- between(1, 8, C1), C1 < C.
rook(square(R, C), black, right, square(R, C1)) :- between(1, 8, C1), C1 < C.
rook(square(R, C), white, right, square(R, C1)) :- between(1, 8, C1), C1 > C.
