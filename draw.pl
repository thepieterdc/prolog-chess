:- module(draw, []).

asciiPiece(bishop(black), '\u265D').
asciiPiece(king(black), '\u265A').
asciiPiece(knight(black), '\u265E').
asciiPiece(pawn(black), '\u265F').
asciiPiece(queen(black), '\u265B').
asciiPiece(rook(black), '\u265C').

asciiPiece(bishop(white), '\u2657').
asciiPiece(king(white), '\u2654').
asciiPiece(knight(white), '\u2658').
asciiPiece(pawn(white), '\u2659').
asciiPiece(queen(white), '\u2655').
asciiPiece(rook(white), '\u2656').

drawBoard([R1, R2, R3, R4, R5, R6, R7, R8]) :-
  drawRow(R8), nl, nl,
  drawRow(R7), nl, nl,
  drawRow(R6), nl, nl,
  drawRow(R5), nl, nl,
  drawRow(R4), nl, nl,
  drawRow(R3), nl, nl,
  drawRow(R2), nl, nl,
  drawRow(R1), nl.

drawLine(0, _) :- !.
drawLine(N, C) :- N1 is N-1, write(C), drawLine(N1, C).

drawPiece(nothing) :- !, write('.').
drawPiece(Piece) :- asciiPiece(Piece, AsciiCode), write(AsciiCode).

drawRow([P1, P2, P3, P4, P5, P6, P7, P8]) :-
  write('   '), drawPiece(P1),
  write('   '), drawPiece(P2),
  write('   '), drawPiece(P3),
  write('   '), drawPiece(P4),
  write('   '), drawPiece(P5),
  write('   '), drawPiece(P6),
  write('   '), drawPiece(P7),
  write('   '), drawPiece(P8).
