:- module(movement, []).

:- use_module(board).
:- use_module(pawn, [move//1 as pawn_move]).
:- use_module(rook, [move//1 as rook_move]).

best(State, Move) :-
  phrase(move(State), Moves), nth0(0, Moves, Move).

forward_left(From, Turn, To) :- forward(From, Turn, Fwd), left(Fwd, Turn, To).
forward_right(From, Turn, To) :- forward(From, Turn, Fwd), right(Fwd, Turn, To).

left(square(R, C), black, square(R, C1)) :- C1 is C + 1, C1 =< 8.
left(square(R, C), white, square(R, C1)) :- C1 is C - 1, C1 >= 1.

% move(State) --> pawn_move(State).
move(State) --> rook_move(State).

% checkt niet de square zelf want bij bvb capture kan dit de bedoeling zijn dat die niet free is.
path_clear(Board, From, Color, forward, To) :-
  forward(From, Color, Next),
  path_clear_sub(Board, Next, Color, forward, To).

% subroutine zodat eerste square niet leeg moet zijn want dat zou achterlijk zijn
path_clear_sub(_, Square, _, _, Square).
path_forward_clear_sub(Board, From, Color, To) :-
  board:free(Board, From),
  forward(From, Color, Next),
  path_forward_clear(Board, Next, Color, To).

pawn_capture(From, Turn, To) :- forward_left(From, Turn, To).
pawn_capture(From, Turn, To) :- forward_right(From, Turn, To).

pawn_forward(From, black, To) :- forward(From, black, To).
pawn_forward(square(7, C), black, square(5, C)) :- between(1, 8, C).
pawn_forward(From, white, To) :- forward(From, white, To).
pawn_forward(square(2, C), white, square(4, C)) :- between(1, 8 ,C).

position(square(R, C), black, forward, square(R1, C)) :- R1 is R - 1, R1 >= 1.
position(square(R, C), white, forward, square(R1, C)) :- R1 is R + 1, R1 =< 8.

right(square(R, C), black, square(R, C1)) :- C1 is C - 1, C1 >= 1.
right(square(R, C), white, square(R, C1)) :- C1 is C + 1, C1 =< 8.

% forward/backward
rook(square(R, C), square(R1, C)) :- between(1, 8, R1), R \= R1.
% left/right
rook(square(R, C), square(R, C1)) :- between(1, 8, C1), C \= C1.
