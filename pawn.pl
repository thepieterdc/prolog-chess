:- module(pawn, []).

:- use_module(board).
:- use_module(fen).
:- use_module(movement).
:- use_module(state).

at(Board, Color, Square) :- board:piece_at(Board, Square, piece(pawn, Color)).

% Regular pawn moves.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:pawn_forward(Square, Turn, Destination),

    \+ promotion_square(Turn, Destination),

    movement:path_forward_clear(Board, Square, Turn, Destination),

    board:free(Board, Destination)
  },
  [move(move, Square, Destination)].


% Pawn promotion.
move(State) --> board:square(Square),
  {
    state:board(State, Board),
    state:turn(State, Turn),

    at(Board, Turn, Square),

    movement:pawn_forward(Square, Turn, Destination),

    promotion_square(Turn, Destination),

    movement:path_forward_clear(Board, Square, Turn, Destination),

    board:free(Board, Destination)
  },
  [
    move(promotion, bishop, Square, Destination),
    move(promotion, knight, Square, Destination),
    move(promotion, queen, Square, Destination),
    move(promotion, rook, Square, Destination)
  ].

promotion_square(black, square(1, _)).
promotion_square(white, square(8, _)).
