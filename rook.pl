:- module(rook, []).

:- use_module(board).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

at(Board, Color, Square) :- board:piece_at(Board, Square, piece(rook, Color)).

% Rook capture.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:rook(Square, Turn, Direction, Destination),

    movement:path_clear(Board, Square, Turn, Direction, Destination),

    board:enemy(Board, Destination, Turn)
  },
  [move(capture, Square, Destination)].

% Rook moves.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:rook(Square, Turn, Direction, Destination),

    movement:path_clear(Board, Square, Turn, Direction, Destination),

    board:free(Board, Destination)
  },
  [move(move, Square, Destination)].
