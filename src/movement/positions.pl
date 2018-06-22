:- module(positions, []).

:- dynamic saved_bishop_moves/2.
:- dynamic saved_king_moves/2.
:- dynamic saved_knight_moves/2.
:- dynamic saved_rook_moves/2.

%% bishop_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a bishop can attack a square, given a starting
%  square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
bishop_attacks(From, Direction, To) :- bishop(From, move(From, Direction, To)).

%% bishop_moves(+R/C: square, -Moves: list).
%
%  Gets all bishop moves from the given starting square.
%
%  @param R/C the starting square
%  @param Moves the moves
bishop_moves(R/C, Moves) :-
  % Fetch the moves from the cache.
  saved_bishop_moves(R/C, Moves), !.
bishop_moves(R/C, Moves) :-
  % Generate all moves.
  setof(X, bishop(R/C, X), Moves),
  % Store the moves in the cache for future use.
  assertz(saved_bishop_moves(R/C, Moves)).

%% bishop(+R/C: square, -move: move).
%
%  Generates a bishop move.
%
%  @param R/C the starting square
%  @param move the move
bishop(R/C, move(R/C, down_left, R1/C1)) :-
  % Diagonal offset.
  between(1, 8, I),
  % Diagonal movement.
  R1 is R - I, C1 is C - I,
  % Bounds checking.
  between(1, R, R1), between(1, C, C1).
bishop(R/C, move(R/C, down_right, R1/C1)) :-
  % Diagonal offset.
  between(1, 8, I),
  % Diagonal movement.
  R1 is R - I, C1 is C + I,
  % Bounds checking.
  between(1, R, R1), between(C, 8, C1).
bishop(R/C, move(R/C, up_left, R1/C1)) :-
  % Diagonal offset.
  between(1, 8, I),
  % Diagonal movement.
  R1 is R + I, C1 is C - I,
  % Bounds checking.
  between(R, 8, R1), between(1, C, C1).
bishop(R/C, move(R/C, up_right, R1/C1)) :-
  % Diagonal offset.
  between(1, 8, I),
  % Diagonal movement.
  R1 is R + I, C1 is C + I,
  % Bounds checking.
  between(R, 8, R1), between(C, 8, C1).

%% king_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a king can attack a square, given a starting square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
king_attacks(From, To) :- king(From, move(From, To)).

%% king_moves(+R/C: square, -Moves: list).
%
%  Gets all king moves from the given starting square.
%
%  @param R/C the starting square
%  @param Moves the moves
king_moves(R/C, Moves) :-
  % Fetch the moves from the cache.
  saved_king_moves(R/C, Moves), !.
king_moves(R/C, Moves) :-
  % Generate all moves.
  setof(X, king(R/C, X), Moves),
  % Store the moves in the cache.
  assertz(saved_king_moves(R/C, Moves)).

%% king(+R/C: square, -move: move).
%
%  Generates a king move.
%
%  @param R/C the starting square
%  @param move the move
king(R/C, move(R/C, R1/C1)) :- R < 8, C < 8, R1 is R + 1, C1 is C + 1.
king(R/C, move(R/C, R1/C)) :- R < 8, R1 is R + 1.
king(R/C, move(R/C, R1/C1)) :- R < 8, C > 1, R1 is R + 1, C1 is C - 1.
king(R/C, move(R/C, R/C1)) :- C < 8, C1 is C + 1.
king(R/C, move(R/C, R/C1)) :- C > 1, C1 is C - 1.
king(R/C, move(R/C, R1/C1)) :- R > 1, C < 8, R1 is R - 1, C1 is C + 1.
king(R/C, move(R/C, R1/C)) :- R > 1, R1 is R - 1.
king(R/C, move(R/C, R1/C1)) :- R > 1, C > 1, R1 is R - 1, C1 is C - 1.

%% knight_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a knight can attack a square, given a starting
%  square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
knight_attacks(From, To) :- knight(From, move(From, To)).

%% knight_moves(+R/C: square, -Moves: list).
%
%  Gets all knight moves from the given starting square.
%
%  @param R/C the starting square
%  @param Moves the moves
knight_moves(R/C, Moves) :-
  % Fetch the moves from the cache.
  saved_knight_moves(R/C, Moves), !.
knight_moves(R/C, Moves) :-
  % Generate all moves.
  setof(X, knight(R/C, X), Moves),
  % Store the moves in the cache for future use.
  assertz(saved_knight_moves(R/C, Moves)).

%% knight(+R/C: square, -move: move).
%
%  Generates a knight move.
%
%  @param R/C the starting square
%  @param move the move
knight(R/C, move(R/C, R1/C1)) :- R < 7, C < 8, R1 is R + 2, C1 is C + 1.
knight(R/C, move(R/C, R1/C1)) :- R < 7, C > 1, R1 is R + 2, C1 is C - 1.
knight(R/C, move(R/C, R1/C1)) :- R < 8, C < 7, R1 is R + 1, C1 is C + 2.
knight(R/C, move(R/C, R1/C1)) :- R < 8, C > 2, R1 is R + 1, C1 is C - 2.
knight(R/C, move(R/C, R1/C1)) :- R > 1, C < 7, R1 is R - 1, C1 is C + 2.
knight(R/C, move(R/C, R1/C1)) :- R > 1, C > 2, R1 is R - 1, C1 is C - 2.
knight(R/C, move(R/C, R1/C1)) :- R > 2, C < 8, R1 is R - 2, C1 is C + 1.
knight(R/C, move(R/C, R1/C1)) :- R > 2, C > 1, R1 is R - 2, C1 is C - 1.

%% pawn_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a pawn can attack a square, given a starting square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
pawn_attacks(R/C, Turn, R1/C1) :- pawn_capture(R/C, Turn, move(R/C, R1/C1)).

%% pawn_capture(+R/C: square, +Turn: turn, -move: move).
%
%  Generates a pawn move.
%
%  @param R/C the starting square
%  @param Turn the turn
%  @param move the move
pawn_capture(R/C, black, move(R/C, R1/C1)) :-
  R > 1, C < 8, R1 is R - 1, C1 is C + 1.
pawn_capture(R/C, black, move(R/C, R1/C1)) :-
  R > 1, C > 1, R1 is R - 1, C1 is C - 1.
pawn_capture(R/C, white, move(R/C, R1/C1)) :-
  R < 8, C < 8, R1 is R + 1, C1 is C + 1.
pawn_capture(R/C, white, move(R/C, R1/C1)) :-
  R < 8, C > 1, R1 is R + 1, C1 is C - 1.

%% pawn_enpassant(+R/C: square, +Turn: turn, -move: move).
%
%  Generates a pawn en-passant move.
%
%  @param R/C the initial square
%  @param Turn the turn
%  @param move the move
pawn_enpassant(7/C, black, move(7/C, 6/C, 5/C)) :- between(1, 8, C).
pawn_enpassant(2/C, white, move(2/C, 3/C, 4/C)) :- between(1, 8 ,C).

%% pawn(+R/C: square, +Turn: turn, -move: move).
%
%  Generates a pawn move.
%
%  @param R/C the starting square
%  @param Turn the turn
%  @param move the move
pawn(R/C, black, move(R/C, R1/C)) :- R > 1, R1 is R - 1.
pawn(R/C, white, move(R/C, R1/C)) :- R < 8, R1 is R + 1.

%% queen_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a queen can attack a square, given a starting square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
queen_attacks(From, Direction, To) :- bishop(From, move(From, Direction, To)), !.
queen_attacks(From, Direction, To) :- rook(From, move(From, Direction, To)).

%% queen_moves(+R/C: square, -Moves: list).
%
%  Gets all queen moves from the given starting square.
%
%  @param R/C the starting square
%  @param Moves the moves
queen_moves(R/C, Moves) :-
  % Fetch the bishop from the cache.
  saved_bishop_moves(R/C, BishopMoves),
  % Fetch the rook from the cache.
  saved_rook_moves(R/C, RookMoves),
  % Merge both move sets.
  append(BishopMoves, RookMoves, Moves), !.
queen_moves(R/C, Moves) :-
  % Generate all bishop moves.
  setof(X, bishop(R/C, X), BishopMoves),
  % Generate all rook moves.
  setof(X, rook(R/C, X), RookMoves),
  % Store the bishop moves in the cache for future use.
  assertz(saved_bishop_moves(R/C, BishopMoves)),
  % Store the rook moves in the cache for future use.
  assertz(saved_rook_moves(R/C, RookMoves)),
  % Merge both move sets.
  append(BishopMoves, RookMoves, Moves).

%% rook_attacks(+From: square, +Direction: direction, +To: square).
%
%  Verifies whether or not a rook can attack a square, given a starting square.
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
rook_attacks(From, Direction, To) :- rook(From, move(From, Direction, To)).

%% rook_moves(+R/C: square, -Moves: list).
%
%  Gets all rook moves from the given starting square.
%
%  @param R/C the starting square
%  @param Moves the moves
rook_moves(R/C, Moves) :-
  % Fetch the moves from the cache.
  saved_rook_moves(R/C, Moves), !.
rook_moves(R/C, Moves) :-
  % Generate all moves.
  setof(X, rook(R/C, X), Moves),
  % Store the moves in the cache for future use.
  assertz(saved_rook_moves(R/C, Moves)).

%% rook(+R/C: square, -move: move).
%
%  Generates a rook move.
%
%  @param R/C the starting square
%  @param move the move
rook(R/C, move(R/C, down, R1/C)) :- B is R - 1, between(1, B, R1).
rook(R/C, move(R/C, left, R/C1)) :- B is C - 1, between(1, B, C1).
rook(R/C, move(R/C, right, R/C1)) :- B is C + 1, between(B, 8, C1).
rook(R/C, move(R/C, up, R1/C)) :- B is R + 1, between(B, 8, R1).
