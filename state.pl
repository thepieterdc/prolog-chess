:- module(state, []).

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
  board:castling_squares(castling(Type, Color), KingSquare, RookSquare),
  board:piece_at(Board, KingSquare, piece(king, Color)),
  board:piece_at(Board, RookSquare, piece(rook, Color)).

castling([_, _, C | _], C).

en_passant([_, _, _, EP | _], EP).

filter_castling(_, [], Done, Done).
filter_castling(Board, [C | Castlings], Before, After) :-
  can_castle(Board, C),
  append(Before, [C], Filtered),
  filter_castling(Board, Castlings, Filtered, After).
filter_castling(Board, [_ | Castlings], After, After) :- filter_castling(Board, Castlings, After, After).

full_count([_, _, _, _, _, FC], FC).

% white is the next player -> black played in this move
inc_fullcount([B, white, C, EP, HC, FC], [B, white, C, EP, HC, FC1]) :-
  succ(FC, FC1).
inc_fullcount([B, black, C, EP, HC, FC], [B, black, C, EP, HC, FC]).

half_count([_, _, _, _, HC | _], HC).

inc_halfcount([B, T, C, EP, _, FC], [B, T, C, EP, 0, FC]).
inc_halfcount([B, T, C, EP, _, FC], piece(pawn, _), [B, T, C, EP, 0, FC]) :- !.
inc_halfcount([B, T, C, EP, HC, FC], _, [B, T, C, EP, HC1, FC]) :-
  succ(HC, HC1).

reset_enpassant([B, T, C, _, HC, FC], [B, T, C, none, HC, FC]).

reset_halfcount([B, T, C, EP, _, FC], [B, T, C, EP, 0, FC]).

turn([_, Turn | _], Turn).

update_board([_, T, C, EP, HC, FC], Board, [Board, T, C, EP, HC, FC]).

update_castling([B, T, C, EP, HC, FC], [B, T, C1, EP, HC, FC]) :-
   filter_castling(B, C, [], C1).

update_enpassant([B, T, C, _, HC, FC], EP, [B, T, C, EP, HC, FC]).

update_turn([B, white, C, EP, HC, FC], [B, black, C, EP, HC, FC]).
update_turn([B, black, C, EP, HC, FC], [B, white, C, EP, HC, FC]).
