:- module(bishop, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% Bishop capture.
move(State, Square, Turn, move(capture, Square, Destination)) :-
  state:board(State, Board),

  movement:bishop(Square, Turn, Direction, Destination),

  movement:path_clear(Board, Square, Turn, Direction, Destination),

  board:enemy(Board, Destination, Turn).

% Bishop walk.
move(State, Square, Turn, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:bishop(Square, Turn, Direction, Destination),

  movement:path_clear(Board, Square, Turn, Direction, Destination),

  board:free(Board, Destination).
