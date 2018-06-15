:- module(knight, []).

:- use_module(board).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

at(Board, Color, Square) :- board:piece_at(Board, Square, piece(knight, Color)).

% Knight capture.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:knight(Square, Destination),

    board:enemy(Board, Destination, Turn)
  },
  [move(capture, Square, Destination)].

% Knight moves.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:knight(Square, Destination),

    board:free(Board, Destination)
  },
  [move(move, Square, Destination)].
