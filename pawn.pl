:- module(pawn, []).

:- use_module(board).
:- use_module(movement).
:- use_module(state).

% Pawn capture.
move(State, Square, Turn, move(capture, Square, Destination)) :-
  state:board(State, Board),

  movement:pawn_capture(Square, Turn, Destination),

  \+ promotion_square(Destination),

  board:enemy(Board, Destination, Turn).

% Pawn capture+promotion.
move(State, Square, Turn, PromotionMoves) :-
  state:board(State, Board),

  movement:pawn_capture(Square, Turn, Destination),

  promotion_square(Destination),

  board:enemy(Board, Destination, Turn),

  findall(Move, promotion_move(Move, Square, Destination), PromotionMoves).

% Regular pawn moves.
move(State, Square, Turn, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:pawn(Square, Turn, Destination),

  \+ promotion_square(Destination),

  movement:path_clear(Board, Square, Turn, forward, Destination),

  board:free(Board, Destination).

% Pawn promotion.
move(State, Square, Turn, PromotionMoves) :-
  state:board(State, Board),

  movement:pawn(Square, Turn, Destination),

  promotion_square(Destination),

  board:free(Board, Destination),

  findall(Move, promotion_move(Move, Square, Destination), PromotionMoves).

promotion_move(move(promotion, bishop, Square, Destination), Square, Destination).
promotion_move(move(promotion, knight, Square, Destination), Square, Destination).
promotion_move(move(promotion, queen, Square, Destination), Square, Destination).
promotion_move(move(promotion, rook, Square, Destination), Square, Destination).

% turn maakt niet uit want pion kan toch niet achteruit
promotion_square(square(1, _)).
promotion_square(square(8, _)).
