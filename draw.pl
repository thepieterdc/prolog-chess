:- module(draw, []).

:- use_module(state).

asciiPiece(bishop, black, '\u265D').
asciiPiece(king, black, '\u265A').
asciiPiece(knight, black, '\u265E').
asciiPiece(pawn, black, '\u265F').
asciiPiece(queen, black, '\u265B').
asciiPiece(rook, black, '\u265C').

asciiPiece(bishop, white, '\u2657').
asciiPiece(king, white, '\u2654').
asciiPiece(knight, white, '\u2658').
asciiPiece(pawn, white, '\u2659').
asciiPiece(queen, white, '\u2655').
asciiPiece(rook, white, '\u2656').

drawBoard(board(R1, R2, R3, R4, R5, R6, R7, R8)) :-
  write('   '), drawRow(R8), write('    8'), nl, nl,
  write('   '), drawRow(R7), write('    7'), nl, nl,
  write('   '), drawRow(R6), write('    6'), nl, nl,
  write('   '), drawRow(R5), write('    5'), nl, nl,
  write('   '), drawRow(R4), write('    4'), nl, nl,
  write('   '), drawRow(R3), write('    3'), nl, nl,
  write('   '), drawRow(R2), write('    2'), nl, nl,
  write('   '), drawRow(R1), write('    1'), nl, nl,
  write('   a   b   c   d   e   f   g   h'), nl, nl.

drawLine(0, _) :- !.
drawLine(N, C) :- N1 is N-1, write(C), drawLine(N1, C).

drawPiece(none) :- !, write('·').
drawPiece(piece(Type,Color)) :- asciiPiece(Type, Color, AsciiCode), write(AsciiCode).

drawRow(row(P1, P2, P3, P4, P5, P6, P7, P8)) :-
  drawPiece(P1),
  write('   '), drawPiece(P2),
  write('   '), drawPiece(P3),
  write('   '), drawPiece(P4),
  write('   '), drawPiece(P5),
  write('   '), drawPiece(P6),
  write('   '), drawPiece(P7),
  write('   '), drawPiece(P8).

drawState(State) :- state:board(State, Board), drawBoard(Board).
