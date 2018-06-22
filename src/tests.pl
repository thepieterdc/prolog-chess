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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Rook tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(rook).

% Horizontal movement
test(rook_horizontal, [forall((
    between(1, 8, R),
    between(1, 8, C)
  ))]) :-
    findall(Col, positions:rook(R/C, move(R/C, left, R/Col)), LeftMoves),
    findall(Col, positions:rook(R/C, move(R/C, right, R/Col)), RightMoves),
    append(LeftMoves, RightMoves, HorizontalMoves),
    exclude(=(R/C), HorizontalMoves, FilteredMoves),
    length(FilteredMoves, 7).

% Vertical movement
test(rook_vertical, [forall((
    between(1, 8, R),
    between(1, 8, C)
  ))]) :-
    findall(Row, positions:rook(R/C, move(R/C, down, Row/C)), DownMoves),
    findall(Row, positions:rook(R/C, move(R/C, up, Row/C)), UpMoves),
    append(DownMoves, UpMoves, VerticalMoves),
    exclude(=(R/C), VerticalMoves, FilteredMoves),
    length(FilteredMoves, 7).

:- end_tests(rook).

main :-
  load_test_files([rook]),

  show_coverage(run_tests),

  halt.
