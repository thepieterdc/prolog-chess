:- module(fen, []).

:- use_module(library(dcg/basics)).

parse(FenString, State) :-
  phrase(state(State), FenString).

state([Board, Turn, Castling, EnPassant, HalfCount, FullCount]) -->
  board(Board), white, turn(Turn), white, castling(Castling),
  white, en_passant(EnPassant), white, half_count(HalfCount),
  white, full_count(FullCount).

board(board(R1, R2, R3, R4, R5, R6, R7, R8)) -->
  row(R8),"/",row(R7),"/",row(R6),"/",row(R5),
  "/",row(R4),"/",row(R3),"/",row(R2),"/",row(R1).

castling([]) --> "-".

en_passant([]) --> "-".

full_count(N) --> integer(N), {N > 0}.

half_count(N) --> integer(N), {N >= 0}.

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

piece([Piece], Left, Left1) -->
  piece(Piece),
  {
    Left1 is Left-1,
    Left1 >= 0
  }.

piece(Empties, Left, Left1) -->
  digit(Char),
  {
    number_codes(I, [Char]),
    between(1, 8, I),
    length(Empties, I),
    maplist(=(none), Empties),
    Left1 is Left - I,
    Left1 >= 0
  }.

pieces([], 0) --> [].

pieces([H|R], Left) --> piece(H, Left, Left1),  pieces(R, Left1).

row(row(A, B, C, D, E, F, G, H)) -->
  pieces(Row, 8),
  {
    flatten(Row, RowF),
    nth1(1, RowF, A),
    nth1(2, RowF, B),
    nth1(3, RowF, C),
    nth1(4, RowF, D),
    nth1(5, RowF, E),
    nth1(6, RowF, F),
    nth1(7, RowF, G),
    nth1(8, RowF, H)
  }.

turn(black) --> "b".
turn(white) --> "w".
