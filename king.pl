:- module(king, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% King capture.
move(State, Square, Turn, move(capture, Square, Destination)) :-
  state:board(State, Board),

  movement:king(Square, Destination),

  board:enemy(Board, Destination, Turn).

% King walk.
move(State, Square, _, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:king(Square, Destination),

  board:free(Board, Destination).
