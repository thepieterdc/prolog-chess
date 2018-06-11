:- module(pieces, [parse/2]).

bishop(black).
bishop(white).

king(black).
king(white).

knight(black).
knight(white).

pawn(black).
pawn(white).

queen(black).
queen(white).

rook(black).
rook(white).

parse('b', bishop(black)).
parse('B', bishop(white)).
parse('k', king(black)).
parse('K', king(white)).
parse('n', knight(black)).
parse('N', knight(white)).
parse('p', pawn(black)).
parse('P', pawn(white)).
parse('q', queen(black)).
parse('Q', queen(white)).
parse('r', rook(black)).
parse('R', rook(white)).
