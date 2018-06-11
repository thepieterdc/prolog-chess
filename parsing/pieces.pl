:- module(pieces, [parse/2]).

pawn(black).
pawn(white).

parse('p', pawn(black)).
parse('P', pawn(white)).
