:- module(board, [parse/2]).

:- use_module(pieces, [parse/2 as parse_piece]).

parse(FenBoard, X) :-
  split_string(FenBoard, "/", "", X).

  % parse_row(FR1, R1),
  % parse_row(FR2, R2).
  % parse_row(FR3, R3),
  % parse_row(FR4, R4),
  % parse_row(FR5, R5),
  % parse_row(FR6, R6),
  %parse_row(FR7, R7).
  % parse_row(FR8, R8).

parse_row(FenRow, Row) :-
  atom_chars(FenRow, RowSplit),
  parse_row(RowSplit, 8, [], Row).

parse_row([], 0, Done, Done).
