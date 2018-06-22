:- module(fen, []).

:- use_module(library(dcg/basics)).

%% parse(FenString:string, State:state) is semidet
%
%  Parses a given FEN string into a state. Can also perform the reverse
%  conversion operation.
%
%  @param FenString the FEN string
%  @param State the corresponding state
parse(FenString, State) :-
  % Apply the DCG to the string to obtain the state, or reverse.
  phrase(state(State), FenString).

% DCG parser for a state, based on the FEN specifications.
state(state(Board, Turn, Castling, EnPassant, HalfCount, FullCount)) -->
  board(Board), " ", turn(Turn), " ", castlings(Castling),
  " ", en_passant(EnPassant), " ", half_count(HalfCount),
  " ", full_count(FullCount).

% DCG parser for a board.
board(board(R1, R2, R3, R4, R5, R6, R7, R8)) -->
  row(R8),"/",row(R7),"/",row(R6),"/",row(R5),
  "/",row(R4),"/",row(R3),"/",row(R2),"/",row(R1).

% DCG parser for castling.
castlings([]) --> "-", !.
castlings(Cs) --> castling_possibilities(Cs).
% castlings[] kan niet opnieuw gebruikt worden want dan zou K-kq ook geldig zijn
castling_possibilities([]) --> [].
castling_possibilities([C | Cs]) --> castling(C), castling_possibilities(Cs).

% DCG parser for the castling possibilities.
castling(castling(kingside, black)) --> "k".
castling(castling(kingside, white)) --> "K".
castling(castling(queenside, black)) --> "q".
castling(castling(queenside, white)) --> "Q".

% DCG parser for the en passant squares.
en_passant(none) --> "-".
en_passant(3/1) --> "a3".
en_passant(3/2) --> "b3".
en_passant(3/3) --> "c3".
en_passant(3/4) --> "d3".
en_passant(3/5) --> "e3".
en_passant(3/6) --> "f3".
en_passant(3/7) --> "g3".
en_passant(3/8) --> "h3".
en_passant(6/1) --> "a6".
en_passant(6/2) --> "b6".
en_passant(6/3) --> "c6".
en_passant(6/4) --> "d6".
en_passant(6/5) --> "e6".
en_passant(6/6) --> "f6".
en_passant(6/7) --> "g6".
en_passant(6/8) --> "h6".

% DCG parser for the full-move counter.
full_count(N) --> integer(N), {N > 0}.

% DCG parser for the half-move counter.
half_count(N) --> integer(N), {N >= 0}.

% DCG parser for pieces.
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

% DCG parser for pieces on a row.
piece(Piece, Left, Left1) --> piece(Piece), {Left1 is Left-1, Left1 >= 0}.

% DCG parser for pieces on a row.
pieces([], 0) --> [].

% DCG parser for empty squares on a row.
pieces([none, none, none, none, none, none, none, none], 8) --> "8".
pieces([none, none, none, none, none, none, none | X], I) --> "7", !,
  {I1 is I-7, I1 >= 0},
  pieces(X, I1).
pieces([none, none, none, none, none, none | X], I) --> "6", !,
  {I1 is I-6, I1 >= 0},
  pieces(X, I1).
pieces([none, none, none, none, none | X], I) --> "5", !,
  {I1 is I-5, I1 >= 0},
  pieces(X, I1).
pieces([none, none, none, none | X], I) --> "4", !,
  {I1 is I-4, I1 >= 0},
  pieces(X, I1).
pieces([none, none, none | X], I) --> "3", !,
  {I1 is I-3, I1 >= 0},
  pieces(X, I1).
pieces([none, none | X], I) --> "2", !,
  {I1 is I-2, I1 >= 0},
  pieces(X, I1).
pieces([none | X], I) --> "1", !,
  {I1 is I-1, I1 >= 0},
  pieces(X, I1).

% DCG parser for pieces on a row.
pieces([H|R], Left) --> piece(H, Left, Left1), pieces(R, Left1).

% DCG parser (FEN to state) for a row.
row(row(A, B, C, D, E, F, G, H)) --> {var(A), !},
  pieces(Row, 8),
  {
    flatten(Row, [A, B, C, D, E, F, G, H])
  }.

% DCG parser (state to FEN) for a row.
row(row(A, B, C, D, E, F, G, H)) --> {nonvar(A), !},
  pieces([A, B, C, D, E, F, G, H], 8).

% DCG parser for turns.
turn(black) --> "b".
turn(white) --> "w".
