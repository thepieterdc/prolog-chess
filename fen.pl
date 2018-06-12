:- module(fen, []).

:- use_module(library(dcg/basics)).

parse(FenString, State) :-
  write(FenString),
  phrase(state(State), FenString).

state([Board, Turn, Castling, EnPassant, HalfCount, FullCount]) -->
  board(Board), white, turn(Turn), white, castling(Castling),
  white, en_passant(EnPassant), white, half_count(HalfCount),
  white, full_count(FullCount).

board(Rows) --> rows(Rows).

castling([]) --> "-".

en_passant([]) --> "-".

full_count(X) --> integer(X).

half_count(X) --> integer(X).

piece(piece(bishop, black)) --> "b".
piece(piece(bishop, white)) --> "B".
piece(piece(king, black)) --> "k".
piece(piece(king, white)) --> "K".
piece(piece(knight, black)) --> "n".
piece(piece(knight, white)) --> "N".
piece(piece(pawn, black)) --> "p".
piece(piece(pawn, white)) --> "P".
piece(piece(queen, black)) --> "q".
piece(piece(queen, white)) --> "Q".
piece(piece(rook, black)) --> "r".
piece(piece(rook, white)) --> "R".

row(row(A, B, C, D, E, F, G, H)) -->
  piece(A), piece(B), piece(C), piece(D),
  piece(E), piece(F), piece(G), piece(H).

rows(rows(R1, R2, R3, R4, R5, R6, R7, R8)) -->
  row(R8),"/",row(R7),"/",row(R6),"/",row(R5),
  "/",row(R4),"/",row(R3),"/",row(R2),"/",row(R1).

turn(black) --> "b".
turn(white) --> "w".

%
% fen_board([R1, R2, R3, R4, R5, R6, R7, R8], FenBoard) :-
%   fen_row(FR1, R1),
%   fen_row(FR2, R2),
%   fen_row(FR3, R3),
%   fen_row(FR4, R4),
%   fen_row(FR5, R5),
%   fen_row(FR6, R6),
%   fen_row(FR7, R7),
%   fen_row(FR8, R8),
%   concat_atom([FR8, FR7, FR6, FR5, FR4, FR3, FR2, FR1], FenBoard).
%
% fen_row(Row, Row) :-
%   write(Row), nl.
%   % parse_row(FenRow, 8, [], Row).
%
% parse_row([], 0, Done, Done) :- !.
%
% parse_row([FenDigit|FenRest], I, Before, After) :-
%   atom_number(FenDigit, Digit), !,
%   between(1, I, Digit),
%   Left is I - Digit,
%   length(Nothings, Digit),
%   maplist(=(nothing), Nothings),
%   append(Before, Nothings, ParsedNothings),
%   parse_row(FenRest, Left, ParsedNothings, After).
%
% parse_row([FenPiece|FenRest], I, Before, After) :-
%   Left is I - 1,
%   parse_piece(FenPiece, Piece),
%   append(Before, [Piece], ParsedPiece),
%   parse_row(FenRest, Left, ParsedPiece, After).
%
% fen_string([Board, Turn, C, EP, Half, Full], FenStr) :-
%   fen_board(Board, FBoard),
%
%   parse_turn(FTurn, Turn),
%
%   concat_atom([FTurn, C, EP, Half, Full], ' ', FenStr).
%
% parse(FenStr, [Board, Turn, C, EP, HalfCount, FullCount]) :-
%   split_string(FenStr, " ", "", [FBoard, FTurn, C, EP, FHalf, FFull]),
%
%   parse_board(FBoard, Board),
%
%   parse_turn(FTurn, Turn),
%
%   atom_number(FHalf, HalfCount),
%
%   atom_number(FFull, FullCount).
%
% parse_board(FenBoard, [R8, R7, R6, R5, R4, R3, R2, R1]) :-
%   split_string(FenBoard, "/", "", [FR1,FR2,FR3,FR4,FR5,FR6,FR7,FR8]),
%
%   parse_row(FR1, R1),
%   parse_row(FR2, R2),
%   parse_row(FR3, R3),
%   parse_row(FR4, R4),
%   parse_row(FR5, R5),
%   parse_row(FR6, R6),
%   parse_row(FR7, R7),
%   parse_row(FR8, R8).
%
% parse_piece('b', bishop(black)).
% parse_piece('B', bishop(white)).
% parse_piece('k', king(black)).
% parse_piece('K', king(white)).
% parse_piece('n', knight(black)).
% parse_piece('N', knight(white)).
% parse_piece('p', piece(pawn, black)).
% parse_piece('P', piece(pawn, white)).
% parse_piece('q', queen(black)).
% parse_piece('Q', queen(white)).
% parse_piece('r', rook(black)).
% parse_piece('R', rook(white)).
%
% parse_row(FenRow, Row) :-
%   atom_chars(FenRow, RowSplit),
%   parse_row(RowSplit, 8, [], Row).
%
% parse_row([], 0, Done, Done) :- !.
%
% parse_row([FenDigit|FenRest], I, Before, After) :-
%   atom_number(FenDigit, Digit), !,
%   between(1, I, Digit),
%   Left is I - Digit,
%   length(Nothings, Digit),
%   maplist(=(nothing), Nothings),
%   append(Before, Nothings, ParsedNothings),
%   parse_row(FenRest, Left, ParsedNothings, After).
%
% parse_row([FenPiece|FenRest], I, Before, After) :-
%   Left is I - 1,
%   parse_piece(FenPiece, Piece),
%   append(Before, [Piece], ParsedPiece),
%   parse_row(FenRest, Left, ParsedPiece, After).
%
% parse_turn("b", black).
% parse_turn("w", white).
