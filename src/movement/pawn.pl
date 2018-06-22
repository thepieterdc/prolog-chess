:- module(pawn, []).

:- use_module('../board').
:- use_module('../movement').
:- use_module('../state').

%% move(+State: state, +Turn: turn, +move:move: -move:move) is semidet
%
%  Formulates a capture move.
%
%  @param State the state
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param move(capture) the capturing move
capture(State, Turn, move(From, To), move(capture, From, To)) :-
  % Extract the board from the state.
  state:board(State, Board),
  % Verify the destination is not a promotion square.
  \+ promotion_square(To),
  % Verify the destination square contains an enemy piece.
  board:enemy(Board, To, Turn).

%% move(+State: state, +Turn: turn, +move: move, -PromotionMoves: list) is
%% semidet
%
%  Formulates a promotion-capture move.
%
%  @param State the state
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param PromotionMoves the promotion moves
capture(State, Turn, move(From, To), PromotionMoves) :-
  % Extract the board from the state.
  state:board(State, Board),
  % Verify the destination is a promotion square.
  promotion_square(To),
  % Verify the destination square contains an enemy piece.
  board:enemy(Board, To, Turn),
  % Get all possible promotions.
  bagof(Move, promotion_move(Move, From, To), PromotionMoves).

%% move(+State: state, +Turn: turn, +move: move, -move: move) is semidet
%
%  Formulates an en-pasant capture move.
%
%  @param S the state
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param move(en_passant) the capturing move
capture(S, Turn, move(SR/SC, DR/DC), move(en_passant, SR/SC, SR/DC, DR/DC)) :-
  % Extract the board from the state.
  state:board(S, Board),
  % Extract the en-passant square from the state.
  state:en_passant(S, DR/DC),
  % Verify the en-passant square contains an enemy pawn.
  board:enemy(Board, SR/DC, Turn),
  % Verify the destination square does not contain any piece.
  board:free(Board, DR/DC).

%% move(+Board: board, +Turn: turn, +move: move, -move: move) is semidet
%
%  Formulates a pawn double walking move, for initial positions.
%
%  @param Board the board
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param move(move) the walking move
move(Board, _, move(From, EP, To), move(move, From, EP, To)) :-
  % Verify the skipped square does not contain any piece.
  board:free(Board, EP),
  % Verify the destination square does not contain any piece.
  board:free(Board, To).

%% move(+Board: board, +Turn: turn, +move: move, PromotionMoves: list) is
%% semidet
%
%  Formulates a promotion move.
%
%  @param Board the board
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param PromotionMoves the promotion moves
move(Board, _, move(From, To), PromotionMoves) :-
  % Verify the destination is a promotion square.
  promotion_square(To),
  % Verify the destination square does not contain any piece.
  board:free(Board, To),
  % Get all possible promotions.
  bagof(Move, promotion_move(Move, From, To), PromotionMoves).

%% move(+Board: board, +Turn: turn, +move: move, -move: move) is semidet
%
%  Formulates a pawn double walking move, for initial positions.
%
%  @param Board the board
%  @param Turn the owner of the pawn
%  @param move the unprocessed move
%  @param move(move) the walking move
move(Board, _, move(From, To), move(move, From, To)) :-
  % Verify the destination is not a promotion square.
  \+ promotion_square(To),
  % Verify the destination square does not contain any piece.
  board:free(Board, To).

%% moves(+State: state, +Square: square, +Turn: turn, -list) is semidet
%
%  Finds all valid moves for a pawn on the given Square.
%
%  @param State the current game state
%  @param Square the square that contains a pawn
%  @param Turn the owner of the pawn
%  @param Moves the resulting available moves
moves(State, Square, Turn, [Moves, Captures, EnPassants]) :-
  % Extract the board from the state.
  state:board(State, Board),

  % Get the pawn move from the current square.
  positions:pawn(Square, Turn, PawnMove),
  % Convert the move to either a move or a promotion.
  convlist(move(Board, Turn), [PawnMove], Moves),

  % Find all capture moves.
  findall(X, positions:pawn_capture(Square, Turn, X), PawnCaptures),
  % Convert the moves to either a capture or a promotion-capture.
  convlist(capture(State, Turn), PawnCaptures, Captures),

  % Find all en-passant moves.
  findall(X, positions:pawn_enpassant(Square, Turn, X), EnPassantMove),
  % Convert the moves to either a walk or a capture.
  convlist(move(Board, Turn), EnPassantMove, EnPassants).

% Valid promotions.
promotion_move(move(promotion, bishop, From, To), From, To).
promotion_move(move(promotion, knight, From, To), From, To).
promotion_move(move(promotion, queen, From, To), From, To).
promotion_move(move(promotion, rook, From, To), From, To).

% Squares that allow pawn promotion.
promotion_square(1/_).
promotion_square(8/_).
