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
all_moves(State, Square, piece(king, Turn), Moves) :- king_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(knight, Turn), Moves) :- knight_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(pawn, Turn), Moves) :- setof(X, pawn_move(State, Square, Turn, X), Moves), !.
all_moves(State, Square, piece(queen, Turn), Moves) :- queen_moves(State, Square, Turn, Moves), !.
all_moves(State, Square, piece(rook, Turn), Moves) :- rook_moves(State, Square, Turn, Moves), !.
all_moves(_, _, _, []).

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

path_next(R/C, down, R1/C) :- R1 is R - 1.
path_next(R/C, down_left, R1/C1) :- R1 is R - 1, C1 is C - 1.
path_next(R/C, down_right, R1/C1) :- R1 is R - 1, C1 is C + 1.
path_next(R/C, left, R/C1) :- C1 is C - 1.
path_next(R/C, right, R/C1) :- C1 is C + 1.
path_next(R/C, up, R1/C) :- R1 is R + 1.
path_next(R/C, up_left, R1/C1) :- R1 is R + 1, C1 is C - 1.
path_next(R/C, up_right, R1/C1) :- R1 is R + 1, C1 is C + 1.
