:- module(board, []).

:- use_module(state).

% A list of all the possible castling squares in this order:
% (castling_type, KingInitial, RookInitial, KingDestination, RookDestination).
castling_squares(castling(kingside, black), 8/5, 8/8, 8/7, 8/6).
castling_squares(castling(kingside, white), 1/5, 1/8, 1/7, 1/6).
castling_squares(castling(queenside, black), 8/5, 8/1, 8/3, 8/6).
castling_squares(castling(queenside, white), 1/5, 1/1, 1/3, 1/4).

%% check(+Board: board, +Player: turn) is semidet
%
%  Validates the given player is in check.
%
%  @param Board the board
%  @param Player the player to verify
check(Board, Player) :-
  % Get the location of the player's king.
  piece_at(Board, KingSquare, piece(king, Player)),
  % Get the enemy of the given player.
  state:enemy(Player, Enemy),
  % Verify that the enemy is attacking the king of the given player.
  state:attacking(Board, Enemy, KingSquare), !.

%% clear(+Board: board, +R/C: square, -After: board) is semidet
%
%  Removes a piece from the board.
%
%  @param Before the initial board
%  @param R/C the square at which the piece should be removed
%  @param After the resulting board
clear(Before, R/C, After) :-
  nth1_row(Before, R, RowBefore),
  piece_replace(RowBefore, C, none, RowAfter),
  row_replace(Before, R, RowAfter, After).

%% enemy(+Board: board, +Square: square, +turn) is semidet
%
%  Validates the given square contains any piece that belongs to the given
%  player's enemy.
%
%  @param Board the board
%  @param Square the square to check
%  @param black-white the player
enemy(Board, Square, black) :- piece_at(Board, Square, piece(_, white)).
enemy(Board, Square, white) :- piece_at(Board, Square, piece(_, black)).

%% free(+Board: board, +R/C: square) is semidet
%
%  Succeeds if the given square does not contain any piece.
%
%  @param Board the board
%  @param R/C the square
free(Board, R/C) :-
  % Extract the row from the board.
  nth1_row(Board, R, Row),
  % Verify the piece at the given square is none.
  nth1_piece(Row, C, none).

%% mine(+Board: board, +Square: square, +Player: turn) is semidet
%
%  Validates the given square contains any piece that belongs to the given
%  player.
%
%  @param Board the board
%  @param Square the square to check
%  @param Player the player to match
mine(Board, Square, Player) :- piece_at(Board, Square, piece(_, Player)).

%% move_piece(+Before: board, +From: square, +To: square, -After: board) is
%% semidet
%
%  Moves a piece on the board.
%
%  @param Before the initial board
%  @param From the current square
%  @param To the destination square
%  @param After the resulting board
move_piece(Before, From, To, After) :-
  % Get the piece to replace.
  piece_at(Before, From, Piece),
  % Remove the piece from the square.
  set_piece(Before, From, none, Removed),
  % Put the piece at the destination square.
  set_piece(Removed, To, Piece, After).

%% nth1_piece(+board:board, +integer, +R: -R: row) is det
%
%  Gets the row at the given row number on the board.
%
%  @param board the Board
%  @param integer the row number
%  @param R the row at that row number
nth1_row(board(R, _, _, _, _, _, _, _), 1, R).
nth1_row(board(_, R, _, _, _, _, _, _), 2, R).
nth1_row(board(_, _, R, _, _, _, _, _), 3, R).
nth1_row(board(_, _, _, R, _, _, _, _), 4, R).
nth1_row(board(_, _, _, _, R, _, _, _), 5, R).
nth1_row(board(_, _, _, _, _, R, _, _), 6, R).
nth1_row(board(_, _, _, _, _, _, R, _), 7, R).
nth1_row(board(_, _, _, _, _, _, _, R), 8, R).

%% nth1_piece(+row:row, +integer, +R: -P: piece) is det
%
%  Gets the piece at the given column in the row.
%
%  @param row the row
%  @param integer the column number
%  @param P the piece at that position
nth1_piece(row(P, _, _, _, _, _, _, _), 1, P).
nth1_piece(row(_, P, _, _, _, _, _, _), 2, P).
nth1_piece(row(_, _, P, _, _, _, _, _), 3, P).
nth1_piece(row(_, _, _, P, _, _, _, _), 4, P).
nth1_piece(row(_, _, _, _, P, _, _, _), 5, P).
nth1_piece(row(_, _, _, _, _, P, _, _), 6, P).
nth1_piece(row(_, _, _, _, _, _, P, _), 7, P).
nth1_piece(row(_, _, _, _, _, _, _, P), 8, P).

%% piece_at(+Board:board, +R/C: square, -Piece: piece) is semidet
%
%  Gets the piece at a given square;
%
%  @param Board the board
%  @param R/C the square
%  @param Piece the piece at that square.
piece_at(Board, R/C, Piece) :-
  % Extract the row of the piece from the board.
  nth1_row(Board, R, Row),
  % Extract the column of the piece from the row.
  nth1_piece(Row, C, Piece).

%% piece_replace(+row:row, +integer, +P: Piece, -row:row) is semidet
%
%  Replaces a piece in a row.
%
%  @param row the initial row
%  @param integer the column number to replace
%  @param P the new piece to put on the given column
%  @param row the resulting row
piece_replace(row(_, P2, P3, P4, P5, P6, P7, P8), 1, P, row(P, P2, P3, P4, P5, P6, P7, P8)).
piece_replace(row(P1, _, P3, P4, P5, P6, P7, P8), 2, P, row(P1, P, P3, P4, P5, P6, P7, P8)).
piece_replace(row(P1, P2, _, P4, P5, P6, P7, P8), 3, P, row(P1, P2, P, P4, P5, P6, P7, P8)).
piece_replace(row(P1, P2, P3, _, P5, P6, P7, P8), 4, P, row(P1, P2, P3, P, P5, P6, P7, P8)).
piece_replace(row(P1, P2, P3, P4, _, P6, P7, P8), 5, P, row(P1, P2, P3, P4, P, P6, P7, P8)).
piece_replace(row(P1, P2, P3, P4, P5, _, P7, P8), 6, P, row(P1, P2, P3, P4, P5, P, P7, P8)).
piece_replace(row(P1, P2, P3, P4, P5, P6, _, P8), 7, P, row(P1, P2, P3, P4, P5, P6, P, P8)).
piece_replace(row(P1, P2, P3, P4, P5, P6, P7, _), 8, P, row(P1, P2, P3, P4, P5, P6, P7, P)).

%% row_replace(+board:board, +integer, +R: row, -board:board) is semidet
%
%  Replaces a row on the board.
%
%  @param board the initial board
%  @param integer the row number to replace
%  @param R the new row to put on the given row number
%  @param board the resulting board
row_replace(board(_, R2, R3, R4, R5, R6, R7, R8), 1, R, board(R, R2, R3, R4, R5, R6, R7, R8)).
row_replace(board(R1, _, R3, R4, R5, R6, R7, R8), 2, R, board(R1, R, R3, R4, R5, R6, R7, R8)).
row_replace(board(R1, R2, _, R4, R5, R6, R7, R8), 3, R, board(R1, R2, R, R4, R5, R6, R7, R8)).
row_replace(board(R1, R2, R3, _, R5, R6, R7, R8), 4, R, board(R1, R2, R3, R, R5, R6, R7, R8)).
row_replace(board(R1, R2, R3, R4, _, R6, R7, R8), 5, R, board(R1, R2, R3, R4, R, R6, R7, R8)).
row_replace(board(R1, R2, R3, R4, R5, _, R7, R8), 6, R, board(R1, R2, R3, R4, R5, R, R7, R8)).
row_replace(board(R1, R2, R3, R4, R5, R6, _, R8), 7, R, board(R1, R2, R3, R4, R5, R6, R, R8)).
row_replace(board(R1, R2, R3, R4, R5, R6, R7, _), 8, R, board(R1, R2, R3, R4, R5, R6, R7, R)).

%% set_piece(+Before:board, +R/C: square, +Piece:piece, -After:board) is semidet
%
%  Sets the given piece on the board at square R/C.
%
%  @param Before the initial board
%  @param R/C the square to put the piece on
%  @param Piece the piece to put
%  @param After the resulting board
set_piece(Before, R/C, Piece, After) :-
  % Fetch the row to put the piece on.
  nth1_row(Before, R, RowBefore),
  % Put the piece on the correct row.
  piece_replace(RowBefore, C, Piece, RowAfter),
  % Put the modified row in the resulting board.
  row_replace(Before, R, RowAfter, After).

%% square(R/C) is nondet
%
%  Validates or generates a square on the playfield.
%
%  @param R/C the row and column of the square
square(R/C) --> between(1, 8, R), between(1, 8, C).
square(R/C) :- between(1, 8, R), between(1, 8, C).
