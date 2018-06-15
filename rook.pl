:- module(rook, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% Rook capture.
move(State, Square, Turn, move(capture, Square, Destination)) :-
  state:board(State, Board),

  movement:rook(Square, Turn, Direction, Destination),

  movement:path_clear(Board, Square, Turn, Direction, Destination),

  board:enemy(Board, Destination, Turn).

% Rook walk.
move(State, Square, Turn, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:rook(Square, Turn, Direction, Destination),

  movement:path_clear(Board, Square, Turn, Direction, Destination),

  board:free(Board, Destination).
