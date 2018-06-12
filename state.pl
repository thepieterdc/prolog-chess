:- module(state, []).

board([Board | _], Board).
castling([_, _, C | _], C).
en_passant([_, _, _, EP | _], EP).
full_count([_, _, _, _, _, FC], FC).
half_count([_, _, _, _, HC | _], HC).
turn([_, Turn | _], Turn).
