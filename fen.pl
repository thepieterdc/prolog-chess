:- module(fen, []).

parse(FenStr, [Board, Turn, nothing, nothing, HalfCount, FullCount]) :-
  split_string(FenStr, " ", "", [FBoard, FTurn, _, _, FHalf, FFull]),

  parse_board(FBoard, Board),

  parse_turn(FTurn, Turn),

  atom_number(FHalf, HalfCount),

  atom_number(FFull, FullCount).

parse_board(FenBoard, [R8, R7, R6, R5, R4, R3, R2, R1]) :-
  split_string(FenBoard, "/", "", [FR1,FR2,FR3,FR4,FR5,FR6,FR7,FR8]),

  parse_row(FR1, R1),
  parse_row(FR2, R2),
  parse_row(FR3, R3),
  parse_row(FR4, R4),
  parse_row(FR5, R5),
  parse_row(FR6, R6),
  parse_row(FR7, R7),
  parse_row(FR8, R8).

parse_piece('b', bishop(black)).
parse_piece('B', bishop(white)).
parse_piece('k', king(black)).
parse_piece('K', king(white)).
parse_piece('n', knight(black)).
parse_piece('N', knight(white)).
parse_piece('p', piece(pawn, black)).
parse_piece('P', piece(pawn, white)).
parse_piece('q', queen(black)).
parse_piece('Q', queen(white)).
parse_piece('r', rook(black)).
parse_piece('R', rook(white)).

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

parse_turn("b", black).
parse_turn("w", white).
