:- module(state, []).

:- use_module(board).
:- use_module(movement).

%% apply_move(+Before: state, +Move: move, -After: state).
%
%  Applies the given castling move to the state.
%
%  @param Before the current state
%  @param Move the move to apply
%  @param After the resulting state, after applying the move
apply_move(Before, move(castling, Castling), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),

  %%%% BOARD
  % Update the castling squares.
  board:castling_squares(Castling, KingFrom, RookFrom, KingTo, RookTo),
  % Castle the king.
  board:move_piece(BeforeBoard, KingFrom, KingTo, AfterBoardKing),
  % Castle the rook.
  board:move_piece(AfterBoardKing, RookFrom, RookTo, AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  reset_enpassant(CastlingState, EnPassantState),

  %%%% HALF-MOVE COUNTER
  inc_halfcount(EnPassantState, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).

%% apply_move(+Before: state, +Move: move, -After: state).
%
%  Applies the given capture move to the state.
%
%  @param Before the current state
%  @param Move the move to apply
%  @param After the resulting state, after applying the move
apply_move(Before, move(capture, From, To), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),

  %%%% BOARD
  % Move the piece on the board.
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  reset_enpassant(CastlingState, EnPassantState),

  %%%% HALF-MOVE COUNTER
  reset_halfcount(EnPassantState, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).

%% apply_move(+Before: state, +Move: move, -After: state).
%
%  Applies the given en-passant move to the state.
%
%  @param Before the current state
%  @param Move the move to apply
%  @param After the resulting state, after applying the move
apply_move(Before, move(en_passant, From, EPCapture, To), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),

  %%%% BOARD
  % Move the piece.
  board:move_piece(BeforeBoard, From, To, AfterMove),
  % Clear het en passant square.
  board:clear(AfterMove, EPCapture, AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  reset_enpassant(CastlingState, EnPassantState),

  %%%% HALF-MOVE COUNTER
  reset_halfcount(EnPassantState, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).

%% apply_move(+Before: state, +Move: move, -After: state).
%
%  Applies the given move to the state.
%
%  @param Before the current state
%  @param Move the move to apply
%  @param After the resulting state, after applying the move
apply_move(Before, move(move, From, EPSquare, To), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),

  %%%% BOARD
  % Move the piece.
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  update_enpassant(CastlingState, EPSquare, EnPassantState),

  %%%% HALF-MOVE COUNTER
  reset_halfcount(EnPassantState, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).
apply_move(Before, move(move, From, To), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),

  %%%% BOARD
  % Move the piece.
  board:move_piece(BeforeBoard, From, To, AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  reset_enpassant(CastlingState, EnPassantState),

  %%%% HALF-MOVE COUNTER
  % Get a piece that may be captured en-passant.
  board:piece_at(BeforeBoard, From, MovedPiece),
  inc_halfcount(EnPassantState, MovedPiece, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).

%% apply_move(+Before: state, +Move: move, -After: state).
%
%  Applies the given promotion move to the state.
%
%  @param Before the current state
%  @param Move the move to apply
%  @param After the resulting state, after applying the move
apply_move(Before, move(promotion, Piece, From, To), After) :-
  % Extract the board from the state.
  board(Before, BeforeBoard),
  % Extract the turn from the state.
  turn(Before, Turn),

  %%%% BOARD
  % Clear the current square.
  board:clear(BeforeBoard, From, ClearedBoard),
  % Set the piece on the square.
  board:set_piece(ClearedBoard, To, piece(Piece, Turn), AfterBoard),
  % Save the board.
  update_board(Before, AfterBoard, BoardState),

  %%%% TURN
  update_turn(BoardState, TurnState),

  %%%% CASTLING RIGHTS
  update_castling(TurnState, CastlingState),

  %%%% EN PASSANT
  reset_enpassant(CastlingState, EnPassantState),

  %%%% HALF-MOVE COUNTER
  reset_halfcount(EnPassantState, HCState),

  %%%% FULL-MOVE COUNTER
  inc_fullcount(HCState, After).

%% attacking(+Board: board, +Player: player, +Square: square).
%
%  Validates whether a square is under attack by the given player.
%
%  @param Board the board
%  @param Player the attacker
%  @param Square the square that must be checked
attacking(Board, Player, Square) :-
  % Get a piece of the current player.
  board:piece_at(Board, Position, piece(Type, Player)),
  % Validate if this piece is attacking the square.
  movement:attacking(Board, piece(Type, Player), Position, Square), !.

%% attacking_squares(+Board: board, +Player: player, +Attacked: list).
%
%  Get all squares that are under attack by the given player.
%
%  @param Board the board
%  @param Player the attacker
%  @param Attacked the squares that are under attack
attacking_squares(Board, Player, Attacked) :-
  % Find all squares on the board.
  findall(X, board:square(X), AllSquares),
  % Filter out the squares that are not under attack.
  include(attacking(Board, Player), AllSquares, Attacked).

%% board(+State: state, -Board: board).
%
%  Extract the board from the state.
%
%  @param State the state
%  @param Board the board
board(state(Board, _, _, _, _, _), Board).

can_castle(Board, castling(Type, Color)) :-
  board:castling_squares(castling(Type, Color), KingFrom, RookFrom, _, _),
  board:piece_at(Board, KingFrom, piece(king, Color)),
  board:piece_at(Board, RookFrom, piece(rook, Color)).

%% castling(+State: state, -C: list).
%
%  Extract the castling rights from the state.
%
%  @param State the state
%  @param C the castling rights
castling(state(_, _, C, _, _, _), C).

check(State, Player) :-
  board(State, Board),
  board:check(Board, Player).

enemy(black, white).
enemy(white, black).

%% en_passant(+State: state, -EP: square).
%
%  Extract the en-passant square from the state.
%
%  @param State the state
%  @param EP the en-passant square
en_passant(state(_, _, _, EP, _, _), EP).

%% full_count(+State: state, -FC: integer).
%
%  Extract the full-move counter from the state.
%
%  @param State the state
%  @param FC the full move counter
full_count(state(_, _, _, _, _, FC), FC).

% white is the next player -> black played in this move
inc_fullcount(state(B, white, C, EP, HC, FC), state(B, white, C, EP, HC, FC1)) :-
  succ(FC, FC1).
inc_fullcount(state(B, black, C, EP, HC, FC), state(B, black, C, EP, HC, FC)).

%% half_count(+State: state, -HC: integer).
%
%  Extract the half-move counter from the state.
%
%  @param State the state
%  @param HC the half move counter
half_count(state(_, _, _, _, HC, _), HC).

inc_halfcount(state(B, T, C, EP, HC, FC), state(B, T, C, EP, HC1, FC)) :-
  succ(HC, HC1).
inc_halfcount(state(B, T, C, EP, _, FC), piece(pawn, _), state(B, T, C, EP, 0, FC)) :- !.
inc_halfcount(state(B, T, C, EP, HC, FC), _, state(B, T, C, EP, HC1, FC)) :-
  succ(HC, HC1), HC1 < 75. %remise

piece_at(State, Square, Piece) :- board(State, Board), board:piece_at(Board, Square, Piece).

reset_enpassant(state(B, T, C, _, HC, FC), state(B, T, C, none, HC, FC)).

reset_halfcount(state(B, T, C, EP, _, FC), state(B, T, C, EP, 0, FC)).

%% turn(+State: state, -Turn: turn).
%
%  Extract the turn from the state.
%
%  @param State the state
%  @param Turn the turn
turn(state(_, Turn, _, _, _, _), Turn).

update_board(state(_, T, C, EP, HC, FC), Board, state(Board, T, C, EP, HC, FC)).

update_castling(state(B, T, C, EP, HC, FC), state(B, T, C1, EP, HC, FC)) :-
  include(can_castle(B), C, C1).

update_enpassant(state(B, T, C, _, HC, FC), EP, state(B, T, C, EP, HC, FC)).

update_turn(state(B, _, C, EP, HC, FC), Turn, state(B, Turn, C, EP, HC, FC)).
update_turn(state(B, white, C, EP, HC, FC), state(B, black, C, EP, HC, FC)).
update_turn(state(B, black, C, EP, HC, FC), state(B, white, C, EP, HC, FC)).
