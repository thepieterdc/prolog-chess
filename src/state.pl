:- module(state, []).

:- use_module(board).
:- use_module(movement).

apply_move(Before, move(castling, Castling), After) :-
  board(Before, BeforeBoard),

  % Board
  board:castling_squares(Castling, KingFrom, RookFrom, KingTo, RookTo),
  board:move_piece(BeforeBoard, KingFrom, KingTo, AfterBoardKing),
  board:move_piece(AfterBoardKing, RookFrom, RookTo, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  reset_enpassant(CastlingState, EnPassantState),

  % Half count
  inc_halfcount(EnPassantState, HCState),

  % Full count
  inc_fullcount(HCState, After).


apply_move(Before, move(capture, From, To), After) :-
  board(Before, BeforeBoard),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  reset_enpassant(CastlingState, EnPassantState),

  % Half count
  reset_halfcount(EnPassantState, HCState),

  % Full count
  inc_fullcount(HCState, After).

%en passant capture
apply_move(Before, move(en_passant, From, EPCapture, To), After) :-
  board(Before, BeforeBoard),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterMove),
  board:clear(AfterMove, EPCapture, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  reset_enpassant(CastlingState, EnPassantState),

  % Half count
  reset_halfcount(EnPassantState, HCState),

  % Full count
  inc_fullcount(HCState, After).

%en passant move pawn
apply_move(Before, move(move, From, EPSquare, To), After) :-
  board(Before, BeforeBoard),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  update_enpassant(CastlingState, EPSquare, EnPassantState),

  % Half count
  reset_halfcount(EnPassantState, HCState),

  % Full count
  inc_fullcount(HCState, After).

apply_move(Before, move(move, From, To), After) :-
  board(Before, BeforeBoard),

  board:piece_at(BeforeBoard, From, MovedPiece),

  % Board
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  reset_enpassant(CastlingState, EnPassantState),

  % Half count
  inc_halfcount(EnPassantState, MovedPiece, HCState),

  % Full count
  inc_fullcount(HCState, After).

apply_move(Before, move(promotion, Piece, From, To), After) :-
  board(Before, BeforeBoard),
  turn(Before, Turn),

  % Board
  board:clear(BeforeBoard, From, ClearedBoard),
  board:set_piece(ClearedBoard, To, piece(Piece, Turn), AfterBoard),
  update_board(Before, AfterBoard, BoardState),

  % Turn
  update_turn(BoardState, TurnState),

  % Castling
  update_castling(TurnState, CastlingState),

  % En passant
  reset_enpassant(CastlingState, EnPassantState),

  % Half count
  reset_halfcount(EnPassantState, HCState),

  % Full count
  inc_fullcount(HCState, After).

attacking(Board, Player, Square) :-
  board:piece_at(Board, Position, piece(Type, Player)),
  movement:attacking(Board, piece(Type, Player), Position, Square).

attacking_squares(Board, Player, Attacked) :-
  findall(X, board:square(X), AllSquares),
  include(attacking(Board, Player), AllSquares, Attacked).

board(state(Board, _, _, _, _, _), Board).

can_castle(Board, castling(Type, Color)) :-
  board:castling_squares(castling(Type, Color), KingFrom, RookFrom, _, _),
  board:piece_at(Board, KingFrom, piece(king, Color)),
  board:piece_at(Board, RookFrom, piece(rook, Color)).

castling(state(_, _, C, _, _, _), C).

check(State, Player) :-
  board(State, Board),
  board:check(Board, Player).

enemy(black, white).
enemy(white, black).

en_passant(state(_, _, _, EP, _, _), EP).

full_count(state(_, _, _, _, _, FC), FC).

% white is the next player -> black played in this move
inc_fullcount(state(B, white, C, EP, HC, FC), state(B, white, C, EP, HC, FC1)) :-
  succ(FC, FC1).
inc_fullcount(state(B, black, C, EP, HC, FC), state(B, black, C, EP, HC, FC)).

half_count(state(_, _, _, _, HC, _), HC).

inc_halfcount(state(B, T, C, EP, HC, FC), state(B, T, C, EP, HC1, FC)) :-
  succ(HC, HC1).
inc_halfcount(state(B, T, C, EP, _, FC), piece(pawn, _), state(B, T, C, EP, 0, FC)) :- !.
inc_halfcount(state(B, T, C, EP, HC, FC), _, state(B, T, C, EP, HC1, FC)) :-
  succ(HC, HC1), HC1 < 75. %remise

piece_at(State, Square, Piece) :- board(State, Board), board:piece_at(Board, Square, Piece).

reset_enpassant(state(B, T, C, _, HC, FC), state(B, T, C, none, HC, FC)).

reset_halfcount(state(B, T, C, EP, _, FC), state(B, T, C, EP, 0, FC)).

turn(state(_, Turn, _, _, _, _), Turn).

update_board(state(_, T, C, EP, HC, FC), Board, state(Board, T, C, EP, HC, FC)).

update_castling(state(B, T, C, EP, HC, FC), state(B, T, C1, EP, HC, FC)) :-
  include(can_castle(B), C, C1).

update_enpassant(state(B, T, C, _, HC, FC), EP, state(B, T, C, EP, HC, FC)).

update_turn(state(B, white, C, EP, HC, FC), state(B, black, C, EP, HC, FC)).
update_turn(state(B, black, C, EP, HC, FC), state(B, white, C, EP, HC, FC)).
