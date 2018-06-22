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

filter_same_offset(move(move, R/C, R1/C1)) :-
  between(1, 8, I), I is abs(R1 - R), I is abs(C1 - C).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Bishop tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(bishop).

test(bishop, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(bishop, Color), AfterBoard),
    state:update_board(State, AfterBoard, BishopState),
    movement:all_moves(BishopState, DiagonalMoves),
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
    KingMovesAmt > 0, !.

:- end_tests(king).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Knight tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(knight).

test(knight_capture, [forall((
    between(3, 6, C),
    between(3, 6, R),
    member(R0/C0, [1/2, 2/1]),
    member(ROOffset, [-1, 1]),
    member(COOffset, [-1, 1]),
    member(PieceType, [bishop, king, knight, pawn, queen, rook]),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    RO_Offset is R0 * ROOffset, CO_Offset is C0 * COOffset,
    R1 is RO_Offset + R, C1 is CO_Offset + C,
    board:set_piece(PreBoard, R1/C1, piece(knight, Color), AfterBoard),
    state:enemy(Color, Enemy),
    board:set_piece(AfterBoard, R/C, piece(PieceType, Enemy), EnemyBoard),
    state:update_board(State, EnemyBoard, KnightState),
    movement:all_moves(KnightState, KnightMoves),
    member(move(capture, R1/C1, R/C), KnightMoves), !.

test(knight_movement, [forall((
    between(3, 6, C),
    between(3, 6, R),
    member(R0/C0, [1/2, 2/1]),
    member(ROOffset, [-1, 1]),
    member(COOffset, [-1, 1]),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    RO_Offset is R0 * ROOffset, CO_Offset is C0 * COOffset,
    R1 is RO_Offset + R, C1 is CO_Offset + C,
    board:set_piece(PreBoard, R1/C1, piece(knight, Color), AfterBoard),
    state:update_board(State, AfterBoard, KnightState),
    movement:all_moves(KnightState, KnightMoves),
    member(move(move, R1/C1, R/C), KnightMoves), !.

:- end_tests(knight).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Pawn tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(pawn).

test(pawn_black_double_movement, [forall((
    between(1, 8, C)
  ))]) :-
    fen(empty, black, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, 7/C, piece(pawn, black), AfterBoard),
    state:update_board(State, AfterBoard, PawnState),
    movement:all_moves(PawnState, PawnMoves),
    length(PawnMoves, 2).

test(pawn_white_double_movement, [forall((
    between(1, 8, C)
  ))]) :-
    fen(empty, white, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, 2/C, piece(pawn, white), AfterBoard),
    state:update_board(State, AfterBoard, PawnState),
    movement:all_moves(PawnState, PawnMoves),
    length(PawnMoves, 2).

test(pawn_black_capture, [forall((
    between(1, 8, C),
    between(3, 8, R),
    member(EnemyPiece, [bishop, king, knight, pawn, queen, rook])
  ))]) :-
    fen(empty, black, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(pawn, black), AfterBoard),
    R1 is R - 1,
    (C1 is C - 1, C > 1; C1 is C + 1, C < 8),
    board:set_piece(AfterBoard, R1/C1, piece(EnemyPiece, white), EnemyBoard),
    state:update_board(State, EnemyBoard, PawnState),
    movement:all_moves(PawnState, PawnMoves),
    include(=(move(capture,_,_)), PawnMoves, Captures),
    length(Captures, AmtCaptures),
    AmtCaptures > 0, !.

test(pawn_white_capture, [forall((
    between(1, 8, C),
    between(1, 6, R),
    member(EnemyPiece, [bishop, king, knight, pawn, queen, rook])
  ))]) :-
    fen(empty, white, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(pawn, white), AfterBoard),
    R1 is R + 1,
    (C1 is C - 1, C > 1; C1 is C + 1, C < 8),
    board:set_piece(AfterBoard, R1/C1, piece(EnemyPiece, black), EnemyBoard),
    state:update_board(State, EnemyBoard, PawnState),
    movement:all_moves(PawnState, PawnMoves),
    include(=(move(capture,_,_)), PawnMoves, Captures),
    length(Captures, AmtCaptures),
    AmtCaptures > 0, !.

test(pawn_regular_movement, [forall((
    between(1, 8, C),
    member(R, [3, 4, 5, 6]),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(pawn, Color), AfterBoard),
    state:update_board(State, AfterBoard, PawnState),
    movement:all_moves(PawnState, PawnMoves),
    length(PawnMoves, 1).

:- end_tests(pawn).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Queen tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- begin_tests(queen).

test(queen_diagonal, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(queen, Color), AfterBoard),
    state:update_board(State, AfterBoard, QueenState),
    movement:all_moves(QueenState, DiagonalMoves),
    include(filter_same_offset(), DiagonalMoves, FilteredMoves),
    length(FilteredMoves, AmtMoves),
    AmtMoves > 0.

% Horizontal movement
test(queen_horizontal, [forall((
    between(1, 8, C),
    between(1, 8, R),
    member(Color, [black, white])
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(queen, Color), AfterBoard),
    state:update_board(State, AfterBoard, QueenState),
    movement:all_moves(QueenState, Moves),
    include(filter_row(R), Moves, HorizontalMoves),
    \+ member(move(_, _, R/C), HorizontalMoves),
    length(HorizontalMoves, 7).

% Vertical movement
test(queen_vertical, [forall((
    between(1, 8, C),
    between(1, 8, R)
  ))]) :-
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(queen, Color), AfterBoard),
    state:update_board(State, AfterBoard, QueenState),
    movement:all_moves(QueenState, Moves),
    include(filter_col(C), Moves, VerticalMoves),
    \+ member(move(_, _, R/C), VerticalMoves),
    length(VerticalMoves, 7).

:- end_tests(queen).

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
    fen(empty, Color, State), !,
    state:board(State, PreBoard),
    board:set_piece(PreBoard, R/C, piece(rook, Color), AfterBoard),
    state:update_board(State, AfterBoard, RookState),
    movement:all_moves(RookState, Moves),
    include(filter_col(C), Moves, VerticalMoves),
    \+ member(move(_, _, R/C), VerticalMoves),
    length(VerticalMoves, 7).

:- end_tests(rook).

main :-
  show_coverage(run_tests),

  halt.
