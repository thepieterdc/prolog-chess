#!/usr/bin/swipl

digit(X, Y) :- atom_number(X, Y).

asciiChar('b', '\u265D').
asciiChar('k', '\u265A').
asciiChar('n', '\u265E').
asciiChar('p', '\u265F').
asciiChar('q', '\u265B').
asciiChar('r', '\u265C').

asciiChar('B', '\u2657').
asciiChar('K', '\u2654').
asciiChar('N', '\u2658').
asciiChar('P', '\u2659').
asciiChar('Q', '\u2655').
asciiChar('R', '\u2656').

drawChar(0) :- !.
drawChar(X) :- number(X), !, X1 is X-1, write('.'), drawChar(X1).
drawChar(X) :- digit(X, Y), number(Y), !, X1 is Y-1, write('.'), drawChar(X1).
drawChar(X) :- asciiChar(X, Y), write(Y).
drawChar(X) :- write(X).

drawRow([]) :- !.

drawRow([FenChar|FenList]) :- !, drawChar(FenChar), drawRow(FenList).


drawRow(FenRow) :- atom_chars(FenRow, FenList), drawRow(FenList).

drawBoard([]).
drawBoard([Row|Rows]) :- drawRow(Row), nl, drawBoard(Rows).

% Argument parsi1ng
main :-
  drawBoard(["rnbqkbnr","pppppppp","8","8","8","8","PPPPPPPP","RNBQKBNR"]).
