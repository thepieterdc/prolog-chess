:- module(board, [parse/2]).

:- use_module(pieces, [parse/2 as parse_piece]).

parse(FenBoard, [R1, R2, R3, R4, R5, R6, R7, R8]) :-
  split_string(FenBoard, "/", "", [FR1,FR2,FR3,FR4,FR5,FR6,FR7,FR8]),

  parse_row(FR1, R1),
  parse_row(FR2, R2),
  parse_row(FR3, R3),
  parse_row(FR4, R4),
  parse_row(FR5, R5),
  parse_row(FR6, R6),
  parse_row(FR7, R7),
  parse_row(FR8, R8).

parse_row(FenRow, Row) :-
  atom_chars(FenRow, RowSplit),
  parse_row(RowSplit, 8, [], Row).

parse_row([], 0, Done, Done) :- !.

parse_row([FenDigit|FenRest], I, Before, After) :-
  atom_number(FenDigit, Digit), !,
  between(1, I, Digit),
  Left is I - Digit,
  length(Nothings, Digit),
  maplist(=(nothing), Nothings),
  append(Before, Nothings, ParsedNothings),
  parse_row(FenRest, Left, ParsedNothings, After).

parse_row([FenPiece|FenRest], I, Before, After) :-
  Left is I - 1,
  parse_piece(FenPiece, Piece),
  append(Before, [Piece], ParsedPiece),
  parse_row(FenRest, Left, ParsedPiece, After).
