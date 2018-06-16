:- module(knight, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% Knight capture.
move(State, Square, Turn, move(capture, Square, Destination)) :-
  state:board(State, Board),

  movement:knight(Square, Destination),

  board:enemy(Board, Destination, Turn).

% Knight walk.
move(State, Square, _, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:knight(Square, Destination),

  board:free(Board, Destination).
