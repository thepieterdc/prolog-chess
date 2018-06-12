:- module(board, []).

set_piece(Before, coordinate(X, Y), Piece, After) :-
  nth1(Y, Before, Row),
  replace_nth1(Row, X, Piece, ReplacedRow),
  replace_nth1(Before, Y, ReplacedRow, After).

piece_at(Board, coordinate(X, Y), Piece) :-
  nth1(Y, Board, Row),
  nth1(X, Row, Piece).

replace_nth1([_|Xs], 1, R, [R|Xs]) :- !.
replace_nth1([X|Xs], N, R, [X|Ys]) :-
  N1 is N - 1,
  replace_nth1(Xs, N1, R, Ys).
