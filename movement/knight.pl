:- module(knight, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

% Knight capture.
move(Board, Turn, move(From, To), move(capture, From, To)) :-
  board:enemy(Board, To, Turn), !.

% Knight walk.
move(Board, _, move(From, To), move(move, From, To)) :-
  board:free(Board, To).

moves(State, Square, Turn, Moves) :-
  state:board(State, Board),

  positions:knight_moves(Square, KnightMoves),

  convlist(move(Board, Turn), KnightMoves, Moves).
