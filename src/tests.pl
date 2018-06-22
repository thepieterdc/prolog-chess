#!/usr/bin/env swipl

:- set_prolog_flag(double_quotes, codes).

:- use_module('../src/board').
:- use_module('../src/fen').
:- use_module('../src/movement/positions').
:- use_module('../src/state').

:- initialization(main, main).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Boards
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fens(empty, "8/8/8/8/8/8/8/8 w - - 0 1").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Utilities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fen(Name, State) :-
  fens(Name, Fen),
  fen:parse(Fen, State).

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
%%%% Rook tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(rook).

% Horizontal movement
test(rook_horizontal, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    positions:rook_moves(R/C, Moves),
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

main :-
  show_coverage(run_tests),

  halt.
