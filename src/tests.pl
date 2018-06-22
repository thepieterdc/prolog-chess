#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module('../src/board').
:- use_module('../src/fen').
:- use_module('../src/movement').
:- use_module('../src/movement/king').
:- use_module('../src/movement/positions').
:- use_module('../src/state').

:- initialization(main, main).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Boards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fens(castling_all, "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1").
fens(castling_kingside_blocked, "r3k1Rr/8/8/8/8/8/8/R3K1RR w KQkq - 0 1").
fens(castling_kingside_notallowed, "r3k2r/8/8/8/8/8/8/R3K2R w Qq - 0 1").
fens(castling_queenside_blocked, "r2Rk2r/8/8/8/8/8/8/R2RK2R w KQkq - 0 1").
fens(castling_queenside_notallowed, "r3k2r/8/8/8/8/8/8/R3K2R w Kk - 0 1").
fens(empty, "8/8/8/8/8/8/8/8 w - - 0 1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fen(Name, State) :-
  fens(Name, Fen),
  fen:parse(Fen, State).

fen(Name, Color, State) :-
  fen(Name, PreState),
  state:update_turn(PreState, Color, State).

filter_chebyshev_distance(D, move(move, R/C, R1/C1)) :-
  RD is abs(R-R1), CD is abs(C-C1),
  D is max(RD, CD).

filter_col(Col, move(_, _, _/Col)).

filter_row(Row, move(_, _, Row/_)).

filter_same_offset(move(R/C, _, R1/C1)) :-
  between(1, 8, I), I is abs(R1 - R), I is abs(C1 - C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Bishop tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(bishop).

test(bishop, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    positions:bishop_moves(R/C, DiagonalMoves),
    length(DiagonalMoves, AmtDiagonalMoves),
    include(filter_same_offset(), DiagonalMoves, FilteredMoves),
    length(FilteredMoves, AmtDiagonalMoves).

:- end_tests(bishop).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% King tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(king).

test(king_castle, [forall((
    member(Color, [black, white])
  ))]) :-
    fen(castling_all, Color, State),
    movement:all_moves(State, KingMoves),
    member(move(castling, castling(kingside, Color)), KingMoves),
    member(move(castling, castling(queenside, Color)), KingMoves), !.

test(king_castle_kingside_notallowed, [forall((
    member(Color, [black, white]),
    member(Fen, [castling_kingside_blocked, castling_kingside_notallowed])
  ))]) :-
    fen(Fen, Color, State),
    movement:all_moves(State, KingMoves),
    \+ member(move(castling, castling(kingside, Color)), KingMoves),
    member(move(castling, castling(queenside, Color)), KingMoves), !.

test(king_castle_queenside_notallowed, [forall((
    member(Color, [black, white]),
    member(Fen, [castling_queenside_blocked, castling_queenside_notallowed])
  ))]) :-
    fen(Fen, Color, State), !,
    movement:all_moves(State, KingMoves),
    member(move(castling, castling(kingside, Color)), KingMoves),
    \+ member(move(castling, castling(queenside, Color)), KingMoves), !.

test(king_movement, [forall((
    between(1, 8, C),
    between(1, 8, R),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(king, Color), AfterBoard),
    state:update_board(State, AfterBoard, KingState),
    movement:all_moves(KingState, KingMoves),
    length(KingMoves, KingMovesAmt),
    include(filter_chebyshev_distance(1), KingMoves, FilteredMoves),
    length(FilteredMoves, KingMovesAmt),
    \+ KingMovesAmt is 0, !.

:- end_tests(king).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Rook tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(rook).

% Horizontal movement
test(rook_horizontal, [forall((
    between(1, 8, C),
    between(1, 8, R),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(rook, Color), AfterBoard),
    state:update_board(State, AfterBoard, RookState),
    movement:all_moves(RookState, Moves),
    include(filter_row(R), Moves, HorizontalMoves),
    \+ member(move(_, _, R/C), HorizontalMoves),
    length(HorizontalMoves, 7).

% Vertical movement
test(rook_vertical, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    positions:rook_moves(R/C, Moves),
    include(filter_col(C), Moves, VerticalMoves),
    \+ member(move(_, _, R/C), VerticalMoves),
    length(VerticalMoves, 7).

:- end_tests(rook).

% docs[minimax, movement, state]
% knight tests
% queen tests
% pawn tests

main :-
  show_coverage(run_tests),

  halt.
