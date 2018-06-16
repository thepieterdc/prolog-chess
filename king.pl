:- module(king, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% Castling
move(State, Square, Turn, move(castling, castling(Side, Turn))) :-
  board:castling_squares(castling(Side, Turn), Square, RookSquare, _, _),

  state:board(State, Board),
  state:castling(State, Castlings),

  member(castling(Side, Turn), Castlings),

  (
    movement:path_clear(Board, Square, Turn, left, RookSquare);
    movement:path_clear(Board, Square, Turn, right, RookSquare)
  ).

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
