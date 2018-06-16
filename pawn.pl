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

  bagof(Move, promotion_move(Move, Square, Destination), PromotionMoves).

% En passant capture, enemysquare is plaats van stuk dat naast mij staat dat ik gepakt heb
move(State, square(SR, SC), Turn, move(en_passant, square(SR, SC), square(SR, EC), square(ER, EC))) :-
  state:board(State, Board),

  state:en_passant(State, square(ER, EC)),

  movement:pawn_capture(square(SR, SC), Turn, square(ER, EC)),

  % stuk dat geslagen moet worden staat naast mij.
  board:enemy(Board, square(SR, EC), Turn),

  board:free(Board, square(ER, EC)).

% En passant movement
move(State, Square, Turn, move(move, Square, EPSquare, Destination)) :-
  state:board(State, Board),

  movement:pawn_enpassant(Square, Turn, EPSquare, Destination),

  % hier on ook path_clear voor gebruikt worden maar owell
  board:free(Board, EPSquare),

  board:free(Board, Destination).

% Regular pawn moves.
move(State, Square, Turn, move(move, Square, Destination)) :-
  state:board(State, Board),

  movement:pawn(Square, Turn, Destination),

  \+ promotion_square(Destination),

  board:free(Board, Destination).

% Pawn promotion.
move(State, Square, Turn, PromotionMoves) :-
  state:board(State, Board),

  movement:pawn(Square, Turn, Destination),

  promotion_square(Destination),

  board:free(Board, Destination),

  bagof(Move, promotion_move(Move, Square, Destination), PromotionMoves).

promotion_move(move(promotion, bishop, Square, Destination), Square, Destination).
promotion_move(move(promotion, knight, Square, Destination), Square, Destination).
promotion_move(move(promotion, queen, Square, Destination), Square, Destination).
promotion_move(move(promotion, rook, Square, Destination), Square, Destination).

% turn maakt niet uit want pion kan toch niet achteruit
promotion_square(square(1, _)).
promotion_square(square(8, _)).
