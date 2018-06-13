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

pack([]) --> [].
pack([X]) --> [[X]].
pack([none, Piece | R]) --> {Piece \= none}, [[none]], pack([Piece | R]).
pack([none, none | R]) --> pack([none | R], [none]).
pack([none, none | R], Packed) --> pack([none | R], [none|Packed]).
pack([none, Piece | R], Packed) --> {Piece \= none}, [[none | Packed]], pack([Piece | R]).
pack([none, none], Packed) --> [[none,none|Packed]].
