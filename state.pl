:- module(state, []).

:- use_module(board).

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

board([Board | _], Board).

can_castle(Board, castling(Type, Color)) :-
  board:castling_squares(castling(Type, Color), KingFrom, RookFrom, _, _),
  board:piece_at(Board, KingFrom, piece(king, Color)),
  board:piece_at(Board, RookFrom, piece(rook, Color)).

castling([_, _, C | _], C).

en_passant([_, _, _, EP | _], EP).

full_count([_, _, _, _, _, FC], FC).

% white is the next player -> black played in this move
inc_fullcount([B, white, C, EP, HC, FC], [B, white, C, EP, HC, FC1]) :-
  succ(FC, FC1).
inc_fullcount([B, black, C, EP, HC, FC], [B, black, C, EP, HC, FC]).

half_count([_, _, _, _, HC | _], HC).

inc_halfcount([B, T, C, EP, HC, FC], [B, T, C, EP, HC1, FC]) :-
  succ(HC, HC1).
inc_halfcount([B, T, C, EP, _, FC], piece(pawn, _), [B, T, C, EP, 0, FC]) :- !.
inc_halfcount([B, T, C, EP, HC, FC], _, [B, T, C, EP, HC1, FC]) :-
  succ(HC, HC1).

piece_at(State, Square, Piece) :- board(State, Board), board:piece_at(Board, Square, Piece).

reset_enpassant([B, T, C, _, HC, FC], [B, T, C, none, HC, FC]).

reset_halfcount([B, T, C, EP, _, FC], [B, T, C, EP, 0, FC]).

turn([_, Turn | _], Turn).

update_board([_, T, C, EP, HC, FC], Board, [Board, T, C, EP, HC, FC]).

update_castling([B, T, C, EP, HC, FC], [B, T, C1, EP, HC, FC]) :- include(can_castle(B), C, C1).

update_enpassant([B, T, C, _, HC, FC], EP, [B, T, C, EP, HC, FC]).

update_turn([B, white, C, EP, HC, FC], [B, black, C, EP, HC, FC]).
update_turn([B, black, C, EP, HC, FC], [B, white, C, EP, HC, FC]).
