:- module(rook, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

% Rook capture.
move(Board, Turn, move(From, _, To), move(capture, From, To)) :-
  board:enemy(Board, To, Turn), !.

% Rook walk.
move(Board, _, move(From, _, To), move(move, From, To)) :-
  board:free(Board, To).

moves(State, Square, Turn, Moves) :-
  state:board(State, Board),

  positions:rook_moves(Square, RookMoves),

  include(movement:path_clear(Board), RookMoves, FilteredMoves),

  convlist(move(Board, Turn), FilteredMoves, Moves).
