:- module(board, []).

%king/rook/kingdest/rookdest%
castling_squares(castling(kingside, black), square(8, 5), square(8, 8), square(8, 7), square(8, 6)).
castling_squares(castling(kingside, white), square(1, 5), square(1, 8), square(1, 7), square(1, 6)).
castling_squares(castling(queenside, black), square(8, 5), square(8, 1), square(8, 3), square(8, 6)).
castling_squares(castling(queenside, white), square(1, 5), square(1, 1), square(1, 3), square(1, 4)).

clear(Before, square(R, C), After) :-
  nth1_row(Before, R, RowBefore),
  piece_replace(RowBefore, C, none, RowAfter),
  row_replace(Before, R, RowAfter, After).

enemy(Board, Square, black) :- piece_at(Board, Square, piece(_, white)).
enemy(Board, Square, white) :- piece_at(Board, Square, piece(_, black)).

free(Board, square(R, C)) :-
  nth1_row(Board, R, Row),
  nth1_piece(Row, C, none).

mine(Board, Square, Player) :- piece_at(Board, Square, piece(_, Player)).

move_piece(Before, From, To, After) :-
  piece_at(Before, From, Piece),
  set_piece(Before, From, none, Removed),
  set_piece(Removed, To, Piece, After).

nth1_row(board(R, _, _, _, _, _, _, _), 1, R).
nth1_row(board(_, R, _, _, _, _, _, _), 2, R).
nth1_row(board(_, _, R, _, _, _, _, _), 3, R).
nth1_row(board(_, _, _, R, _, _, _, _), 4, R).
nth1_row(board(_, _, _, _, R, _, _, _), 5, R).
nth1_row(board(_, _, _, _, _, R, _, _), 6, R).
nth1_row(board(_, _, _, _, _, _, R, _), 7, R).
nth1_row(board(_, _, _, _, _, _, _, R), 8, R).

nth1_piece(row(P, _, _, _, _, _, _, _), 1, P).
nth1_piece(row(_, P, _, _, _, _, _, _), 2, P).
nth1_piece(row(_, _, P, _, _, _, _, _), 3, P).
nth1_piece(row(_, _, _, P, _, _, _, _), 4, P).
nth1_piece(row(_, _, _, _, P, _, _, _), 5, P).
nth1_piece(row(_, _, _, _, _, P, _, _), 6, P).
nth1_piece(row(_, _, _, _, _, _, P, _), 7, P).
nth1_piece(row(_, _, _, _, _, _, _, P), 8, P).

piece_at(Board, square(R, C), Piece) :-
  nth1_row(Board, R, Row),
  nth1_piece(Row, C, Piece).

piece_replace(row(_, P2, P3, P4, P5, P6, P7, P8), 1, P, row(P, P2, P3, P4, P5, P6, P7, P8)).
piece_replace(row(P1, _, P3, P4, P5, P6, P7, P8), 2, P, row(P1, P, P3, P4, P5, P6, P7, P8)).
piece_replace(row(P1, P2, _, P4, P5, P6, P7, P8), 3, P, row(P1, P2, P, P4, P5, P6, P7, P8)).
piece_replace(row(P1, P2, P3, _, P5, P6, P7, P8), 4, P, row(P1, P2, P3, P, P5, P6, P7, P8)).
piece_replace(row(P1, P2, P3, P4, _, P6, P7, P8), 5, P, row(P1, P2, P3, P4, P, P6, P7, P8)).
piece_replace(row(P1, P2, P3, P4, P5, _, P7, P8), 6, P, row(P1, P2, P3, P4, P5, P, P7, P8)).
piece_replace(row(P1, P2, P3, P4, P5, P6, _, P8), 7, P, row(P1, P2, P3, P4, P5, P6, P, P8)).
piece_replace(row(P1, P2, P3, P4, P5, P6, P7, _), 8, P, row(P1, P2, P3, P4, P5, P6, P7, P)).

row_replace(board(_, R2, R3, R4, R5, R6, R7, R8), 1, R, board(R, R2, R3, R4, R5, R6, R7, R8)).
row_replace(board(R1, _, R3, R4, R5, R6, R7, R8), 2, R, board(R1, R, R3, R4, R5, R6, R7, R8)).
row_replace(board(R1, R2, _, R4, R5, R6, R7, R8), 3, R, board(R1, R2, R, R4, R5, R6, R7, R8)).
row_replace(board(R1, R2, R3, _, R5, R6, R7, R8), 4, R, board(R1, R2, R3, R, R5, R6, R7, R8)).
row_replace(board(R1, R2, R3, R4, _, R6, R7, R8), 5, R, board(R1, R2, R3, R4, R, R6, R7, R8)).
row_replace(board(R1, R2, R3, R4, R5, _, R7, R8), 6, R, board(R1, R2, R3, R4, R5, R, R7, R8)).
row_replace(board(R1, R2, R3, R4, R5, R6, _, R8), 7, R, board(R1, R2, R3, R4, R5, R6, R, R8)).
row_replace(board(R1, R2, R3, R4, R5, R6, R7, _), 8, R, board(R1, R2, R3, R4, R5, R6, R7, R)).

set_piece(Before, square(R, C), Piece, After) :-
  nth1_row(Before, R, RowBefore),
  piece_replace(RowBefore, C, Piece, RowAfter),
  row_replace(Before, R, RowAfter, After).

square(square(R, C)) --> between(1, 8, R), between(1, 8, C).
square(square(R, C)) :- between(1, 8, R), between(1, 8, C).
