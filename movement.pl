:- module(movement, []).

:- use_module(board).
:- use_module('movement/bishop', [moves/4 as bishop_moves]).
:- use_module(king, [move/4 as king_move]).
:- use_module('movement/knight', [moves/4 as knight_moves]).
:- use_module(pawn, [move/4 as pawn_move]).
:- use_module('movement/queen', [moves/4 as queen_moves]).
:- use_module('movement/rook', [moves/4 as rook_moves]).
:- use_module(state).

all_moves(State, Moves) :-
  findall(X, board:square(X), AllSquares),
  maplist(all_moves(State), AllSquares, AllMoves),
  flatten(AllMoves, Moves).

all_moves(State, Square, Moves) :-
  state:turn(State, Turn),
  state:piece_at(State, Square, piece(Type, Turn)), !,
  all_moves(State, Square, piece(Type, Turn), Moves).
all_moves(_, _, []).

all_moves(State, Square, piece(bishop, Turn), Moves) :- bishop_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(king, Turn), Moves) :- setof(X, king_move(State, Square, Turn, X), Moves), !.
all_moves(State, Square, piece(knight, Turn), Moves) :- knight_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(pawn, Turn), Moves) :- setof(X, pawn_move(State, Square, Turn, X), Moves), !.
all_moves(State, Square, piece(queen, Turn), Moves) :- queen_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(rook, Turn), Moves) :- rook_moves(State, Square, Turn, Moves), !.
all_moves(_, _, _, []).

attacking(Board, piece(bishop, _), Current, Target) :-
  bishop(Current, Turn, Direction, Target),
  path_clear(Board, Current, Turn, Direction, Target).
attacking(_, piece(king, _), Current, Target) :- king(Current, Target).
attacking(_, piece(knight, _), Current, Target) :- knight(Current, Target).
attacking(_, piece(pawn, Color), Current, Target) :- pawn_capture(Current, Color, Target).
attacking(Board, piece(queen, _), Current, Target) :-
  queen(Current, Turn, Direction, Target),
  path_clear(Board, Current, Turn, Direction, Target).
attacking(Board, piece(rook, _), Current, Target) :-
  rook(Current, Turn, Direction, Target),
  path_clear(Board, Current, Turn, Direction, Target).

chebyshev_distance(R1/C1, R2/C2, I) :-
  X is abs(C1 - C2),
  Y is abs(R1 - R2),
  I is max(X, Y).

king(R/C, R1/C1) :-
  between(1, 8, R1), between(1, 8, C1),
  chebyshev_distance(R/C, R1/C1, 1).

% checkt niet de laatste square zelf want bij bvb capture kan dit de bedoeling zijn dat die niet free is.
path_clear(_, move(Square, _, Square)) :- !.

path_clear(Board, move(From, Direction, To)) :-
  path_next(From, Direction, Next),
  path_clear_sub(Board, move(Next, Direction, To)).

% subroutine zodat eerste square niet leeg moet zijn want dat zou achterlijk zijn
path_clear_sub(_, move(Square, _, Square)) :- !.

path_clear_sub(Board, move(From, Direction, To)) :-
  board:free(Board, From),
  path_next(From, Direction, Next),
  path_clear_sub(Board, move(Next, Direction, To)).

% TODO DOE DEZE WEG
% checkt niet de square zelf want bij bvb capture kan dit de bedoeling zijn dat die niet free is.
path_clear(Board, From, Color, Direction, To) :-
  position(From, Color, Direction, Next),
  path_clear_sub(Board, Next, Color, Direction, To).

% subroutine zodat eerste square niet leeg moet zijn want dat zou achterlijk zijn
path_clear_sub(_, Square, _, _, Square).
path_clear_sub(Board, From, Color, Direction, To) :-
  board:free(Board, From),
  position(From, Color, Direction, Next),
  path_clear_sub(Board, Next, Color, Direction, To).

path_next(R/C, down, R1/C) :- R1 is R - 1.
path_next(R/C, down_left, R1/C1) :- R1 is R - 1, C1 is C - 1.
path_next(R/C, down_right, R1/C1) :- R1 is R - 1, C1 is C + 1.
path_next(R/C, left, R/C1) :- C1 is C - 1.
path_next(R/C, right, R/C1) :- C1 is C + 1.
path_next(R/C, up, R1/C) :- R1 is R + 1.
path_next(R/C, up_left, R1/C1) :- R1 is R + 1, C1 is C - 1.
path_next(R/C, up_right, R1/C1) :- R1 is R + 1, C1 is C + 1.


pawn_capture(From, Color, To) :- position(From, Color, forward_left, To).
pawn_capture(From, Color, To) :- position(From, Color, forward_right, To).

pawn_enpassant(7/C, black, 6/C, 5/C) :- between(1, 8, C).
pawn_enpassant(2/C, white, 3/C, 4/C) :- between(1, 8 ,C).

pawn(From, black, To) :- position(From, black, forward, To).
pawn(From, white, To) :- position(From, white, forward, To).

position(R/C, black, backward, R1/C) :- R1 is R + 1, R1 =< 8.
position(R/C, white, backward, R1/C) :- R1 is R - 1, R1 >= 1.
position(R/C, black, backward_left, R1/C1) :- R1 is R + 1, C1 is C + 1, R1 =< 8, C1 =< 8.
position(R/C, white, backward_left, R1/C1) :- R1 is R - 1, C1 is C - 1, R1 >= 1, C1 >= 1.
position(R/C, black, backward_right, R1/C1) :- R1 is R + 1, C1 is C - 1, R1 =< 8, C1 >= 1.
position(R/C, white, backward_right, R1/C1) :- R1 is R - 1, C1 is C + 1, R1 >= 1, C1 =< 8.
position(R/C, black, forward, R1/C) :- R1 is R - 1, R1 >= 1.
position(R/C, white, forward, R1/C) :- R1 is R + 1, R1 =< 8.
position(R/C, black, forward_left, R1/C1) :- R1 is R - 1, C1 is C + 1, R1 >= 1, C1 =< 8.
position(R/C, white, forward_left, R1/C1) :- R1 is R + 1, C1 is C - 1, R1 =< 8, C1 >= 1.
position(R/C, black, forward_right, R1/C1) :- R1 is R - 1, C1 is C - 1, R1 >= 1, C1 >= 1.
position(R/C, white, forward_right, R1/C1) :- R1 is R + 1, C1 is C + 1, R1 =< 8, C1 =< 8.
position(R/C, black, left, R/C1) :- C1 is C + 1, C1 =< 8.
position(R/C, white, left, R/C1) :- C1 is C - 1, C1 >= 1.
position(R/C, black, right, R/C1) :- C1 is C - 1, C1 >= 1.
position(R/C, white, right, R/C1) :- C1 is C + 1, C1 =< 8.

random_move(State, Move) :-
  all_moves(State, Moves),
  length(Moves, AmountMoves),
  random_between(1, AmountMoves, RandomMove),
  nth1(RandomMove, Moves, Move).
