:- module(queen, []).

:- use_module(board).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

at(Board, Color, Square) :- board:piece_at(Board, Square, piece(queen, Color)).

% Queen capture.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:queen(Square, Turn, Direction, Destination),

    movement:path_clear(Board, Square, Turn, Direction, Destination),

    board:enemy(Board, Destination, Turn)
  },
  [move(capture, Square, Destination)].

% Queen moves.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:queen(Square, Turn, Direction, Destination),

    movement:path_clear(Board, Square, Turn, Direction, Destination),

    board:free(Board, Destination)
  },
  [move(move, Square, Destination)].
