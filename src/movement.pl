:- module(movement, []).

:- use_module(board).
:- use_module('movement/bishop', [moves/4 as bishop_moves]).
:- use_module('movement/king', [moves/4 as king_moves]).
:- use_module('movement/knight', [moves/4 as knight_moves]).
:- use_module('movement/pawn', [moves/4 as pawn_move]).
:- use_module('movement/positions').
:- use_module('movement/queen', [moves/4 as queen_moves]).
:- use_module('movement/rook', [moves/4 as rook_moves]).
:- use_module(state).

%% all_moves(+State: state, +Square: square, +Piece: piece, -Moves: list).
%
%  Gets all available moves for the given
%
%  @param From the starting square
%  @param Direction the direction of the move
%  @param To the destination square
all_moves(State, Moves) :-
  % Find all moves on the board.
  findall(X, board:square(X), AllSquares),
  % Generate all moves for every square.
  maplist(all_moves(State), AllSquares, AllMoves),
  % Flatten the found moves.
  flatten(AllMoves, Moves).

%% all_moves(+State: state, +Square: square, -Moves: list).
%
%  Gets all available moves for the given location.
%
%  @param State the current state
%  @param Square the square to generate moves for
%  @param Moves the moves found
all_moves(State, Square, Moves) :-
  % Get the current player.
  state:turn(State, Turn),
  % Get the piece at the given square, only generate moves for the current
  % player.
  state:piece_at(State, Square, piece(Type, Turn)), !,
  % Get all moves.
  all_moves(State, Square, piece(Type, Turn), Moves).
all_moves(_, _, []).

%% all_moves(+State: state, +Square: square, +Piece: piece, -Moves: list).
%
%  Gets all available moves for the given piece at the given location.
%
%  @param State the current state
%  @param Square the square to generate moves for
%  @param Piece the piece at the given square
%  @param Moves the moves found
all_moves(State, Square, piece(bishop, Turn), Moves) :-
  bishop_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(knight, Turn), Moves) :-
  knight_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(king, Turn), Moves) :-
  king_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(pawn, Turn), Moves) :-
  pawn_move(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(queen, Turn), Moves) :-
  queen_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(rook, Turn), Moves) :-
  rook_moves(State, Square, Turn, Moves), !.
all_moves(_, _, _, []).

%% attacking(+Board: board, +Piece: piece, +Current: square, -Target: square).
%
%  Gets whether a square can attack another square.
%
%  @param Board the board
%  @param Piece the piece at the given Current square
%  @param Current the starting piece
%  @param Target the target to attack
attacking(Board, piece(bishop, _), Current, Target) :-
  positions:bishop_attacks(Current, Direction, Target),
  path_clear(Board, move(Current, Direction, Target)).
attacking(_, piece(king, _), Current, Target) :-
  positions:king_attacks(Current, Target).
attacking(_, piece(knight, _), Current, Target) :-
  positions:knight_attacks(Current, Target).
attacking(_, piece(pawn, Color), Current, Target) :-
  positions:pawn_attacks(Current, Color, Target).
attacking(Board, piece(queen, _), Current, Target) :-
  positions:queen_attacks(Current, Direction, Target),
  path_clear(Board, move(Current, Direction, Target)).
attacking(Board, piece(rook, _), Current, Target) :-
  positions:rook_attacks(Current, Direction, Target),
  path_clear(Board, move(Current, Direction, Target)).

%% path_clear(+Board: board, +Path: move).
%
%  Verifies that a path contains only empty squares.
%
%  @param Board the board
%  @param Path the path containing the path to follow
path_clear(_, move(Square, _, Square)) :- !.
path_clear(Board, move(From, Direction, To)) :-
  % Get the next square on the path.
  path_next(From, Direction, Next),
  % Verify this square is empty using the subroutine.
  path_clear_sub(Board, move(Next, Direction, To)).

%% path_clear_sub(+Board: board, +Path: move).
%
%  Verifies that a path contains only empty squares, subroutine. This routine is
%  required, because else the starting square that contains the piece would also
%  need to be empty, which is obviously always false.
%
%  @param Board the board
%  @param Path the path containing the path to follow
path_clear_sub(_, move(Square, _, Square)) :- !.
path_clear_sub(Board, move(From, Direction, To)) :-
  % Verify the square is empty.
  board:free(Board, From),
  % Get the next square on the path.
  path_next(From, Direction, Next),
  % Verify this square is empty using the subroutine.
  path_clear_sub(Board, move(Next, Direction, To)).

%% path_next(+R/C: square, +Direction: direction, -R1/C1: square).
%
%  Get the next square on a path in a given direction.
%
%  @param R/C the current square
%  @param Direction the direction of the path
%  @param R1/C1 the next square
path_next(R/C, down, R1/C) :- R1 is R - 1.
path_next(R/C, down_left, R1/C1) :- R1 is R - 1, C1 is C - 1.
path_next(R/C, down_right, R1/C1) :- R1 is R - 1, C1 is C + 1.
path_next(R/C, left, R/C1) :- C1 is C - 1.
path_next(R/C, right, R/C1) :- C1 is C + 1.
path_next(R/C, up, R1/C) :- R1 is R + 1.
path_next(R/C, up_left, R1/C1) :- R1 is R + 1, C1 is C - 1.
path_next(R/C, up_right, R1/C1) :- R1 is R + 1, C1 is C + 1.
