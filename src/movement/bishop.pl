:- module(bishop, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

% Bishop capture.
move(Board, Turn, move(From, _, To), move(capture, From, To)) :-
  board:enemy(Board, To, Turn), !.

% Bishop walk.
move(Board, _, move(From, _, To), move(move, From, To)) :-
  board:free(Board, To).

moves(State, Square, Turn, Moves) :-
  state:board(State, Board),

  positions:bishop_moves(Square, BishopMoves),

  include(movement:path_clear(Board), BishopMoves, FilteredMoves),

  convlist(move(Board, Turn), FilteredMoves, Moves).
