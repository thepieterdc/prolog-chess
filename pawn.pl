:- module(pawn, []).

:- use_module(board).

pawn_move(State) --> square(State).
