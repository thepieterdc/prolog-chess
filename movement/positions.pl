:- module(positions, []).

:- dynamic saved_bishop_moves/2.
:- dynamic saved_knight_moves/2.
:- dynamic saved_rook_moves/2.

bishop_attacks(From, Direction, To) :- bishop(From, move(From, Direction, To)).

bishop_moves(R/C, Moves) :-
  saved_bishop_moves(R/C, Moves), !.

bishop_moves(R/C, Moves) :-
  setof(X, bishop(R/C, X), Moves),
  assertz(saved_bishop_moves(R/C, Moves)).

bishop(R/C, move(R/C, down_left, R1/C1)) :-
  between(1, 8, I),
  R1 is R - I, C1 is C - I,
  between(1, R, R1), between(1, C, C1).
bishop(R/C, move(R/C, down_right, R1/C1)) :-
  between(1, 8, I),
  R1 is R - I, C1 is C + I,
  between(1, R, R1), between(C, 8, C1).
bishop(R/C, move(R/C, up_left, R1/C1)) :-
  between(1, 8, I),
  R1 is R + I, C1 is C - I,
  between(R, 8, R1), between(1, C, C1).
bishop(R/C, move(R/C, up_right, R1/C1)) :-
  between(1, 8, I),
  R1 is R + I, C1 is C + I,
  between(R, 8, R1), between(C, 8, C1).

knight_attacks(From, To) :- knight(From, To).

knight_moves(R/C, Moves) :-
  saved_knight_moves(R/C, Moves), !.

knight_moves(R/C, Moves) :-
  setof(X, knight(R/C, X), Moves),
  assertz(saved_knight_moves(R/C, Moves)).

knight(R/C, move(R/C, R1/C1)) :- R < 7, C < 8, R1 is R + 2, C1 is C + 1.
knight(R/C, move(R/C, R1/C1)) :- R < 7, C > 1, R1 is R + 2, C1 is C - 1.
knight(R/C, move(R/C, R1/C1)) :- R < 8, C < 7, R1 is R + 1, C1 is C + 2.
knight(R/C, move(R/C, R1/C1)) :- R < 8, C > 2, R1 is R + 1, C1 is C - 2.
knight(R/C, move(R/C, R1/C1)) :- R > 1, C < 7, R1 is R - 1, C1 is C + 2.
knight(R/C, move(R/C, R1/C1)) :- R > 1, C > 2, R1 is R - 1, C1 is C - 2.
knight(R/C, move(R/C, R1/C1)) :- R > 2, C < 8, R1 is R - 2, C1 is C + 1.
knight(R/C, move(R/C, R1/C1)) :- R > 2, C > 1, R1 is R - 2, C1 is C - 1.

queen_attacks(From, Direction, To) :- bishop(From, move(From, Direction, To)), !.
queen_attacks(From, Direction, To) :- rook(From, move(From, Direction, To)).

queen_moves(R/C, Moves) :-
  saved_bishop_moves(R/C, BishopMoves),
  saved_rook_moves(R/C, RookMoves),
  append(BishopMoves, RookMoves, Moves), !.

queen_moves(R/C, Moves) :-
  setof(X, bishop(R/C, X), BishopMoves),
  setof(X, rook(R/C, X), RookMoves),
  assertz(saved_bishop_moves(R/C, BishopMoves)),
  assertz(saved_rook_moves(R/C, RookMoves)),
  append(BishopMoves, RookMoves, Moves).

rook_attacks(From, Direction, To) :- rook(From, move(From, Direction, To)).

rook_moves(R/C, Moves) :-
  saved_rook_moves(R/C, Moves), !.

rook_moves(R/C, Moves) :-
  setof(X, rook(R/C, X), Moves),
  assertz(saved_rook_moves(R/C, Moves)).

rook(R/C, move(R/C, down, R1/C)) :- B is R - 1, between(1, B, R1).
rook(R/C, move(R/C, left, R/C1)) :- B is C - 1, between(1, B, C1).
rook(R/C, move(R/C, right, R/C1)) :- B is C + 1, between(B, 8, C1).
rook(R/C, move(R/C, up, R1/C)) :- B is R + 1, between(B, 8, R1).
