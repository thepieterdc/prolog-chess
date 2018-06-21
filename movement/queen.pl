:- module(queen, []).

:- use_module('../board').
:- use_module('../state').
:- use_module(positions).

% Queen capture.
move(Board, Turn, move(From, _, To), move(capture, From, To)) :-
  board:enemy(Board, To, Turn), !.

% Queen walk.
move(Board, _, move(From, _, To), move(move, From, To)) :-
  board:free(Board, To).

moves(State, Square, Turn, Moves) :-
  state:board(State, Board),

  positions:queen_moves(Square, QueenMoves),

  include(movement:path_clear(Board), QueenMoves, FilteredMoves),

  convlist(move(Board, Turn), FilteredMoves, Moves).
