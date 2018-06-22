:- begin_tests(positions).

:- use_module('../src/movement/positions').

% Horizontal movement
test(rook) :- rook(4/2, _).


:- end_tests(positions).
