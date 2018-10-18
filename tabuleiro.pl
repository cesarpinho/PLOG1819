tab([   [1-8 ,1-7, 1-6 ,1-5, 0, 0, 0, 0],
        [0, 0, 0, 0, 1-12, 1-11, 1-10, 1-9],
        [1-4, 1-3, 1-2, 1-1, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 0, 0, 0, 2-1, 2-2, 2-3, 2-4],
        [2-9, 2-10, 2-11, 2-12, 0, 0, 0, 0],
        [0, 0, 0, 0, 2-5, 2-6, 2-7, 2-8] ]).

display_game(Board, Player) :- 
    tab(Board),
    write('      A     B     C     D     E     F     G     H'), 
    nl,
    print_tab(Board, 1),
    nl,
    write('      A     B     C     D     E     F     G     H'),
    nl,
    write(' ').

print_tab([], _) :- write('   -------------------------------------------------').
print_tab([L|T], NumL) :-
    write('   -------------------------------------------------'),
    nl,
    write(' '),
    write(NumL),
    write(' | '),
    print_line(L),
    write(NumL),
    nl,
    NextL is NumL + 1,
    print_tab(T, NextL).

print_line([]).
print_line([C|L]) :-    
    print_cel(C),
    write(' | '),
    print_line(L).

print_cel(0) :- write('   ').
print_cel(1-X) :- X < 10, write('B'), write(X), write(' ').
print_cel(1-X) :- X >= 10, write('B'), write(X).
print_cel(2-X) :- X < 10, write('W'), write(X), write(' ').
print_cel(2-X) :- X >= 10, write('W'), write(X).