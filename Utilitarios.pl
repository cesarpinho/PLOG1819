:- use_module(library(between)).

empty_cel(0-0).

clr :- write('\33\[2J').

wait_enter :-
    write('Press any key to continue...'),
    new_line(2),
    get_char(_)/* ,
    clr */.

change_player(1, 2).
change_player(2, 1).

isEmpty([]).
isEmpty([_|_]) :- !, fail.

count_lines([_|L], NumL) :-  
    count_lines(L,N), 
    NumL is N + 1.

count_lines([],0).

board_size([H | T], Col, Lin) :-
    count_lines(H, Col),
    count_lines(T, X),
    Lin is X + 1.

space(1) :- write(' ').
space(N) :-
    N > 1,
    write(' '),
    Next is N - 1,
    space(Next).

new_line(1) :- nl.
new_line(N) :-
    N > 1,
    nl,
    Next is N - 1,
    new_line(Next).

not(X) :- X ,! ,fail.
not(_).

replace([X|L], Elem, 0, Col, [Y|L]) :-
    replace(X, Elem, Col, Y).
replace([X|L], Elem, Lin, Col, [X|NewL]) :-
    Lin > 0,
    Lin1 is Lin - 1,
    replace(L, Elem, Lin1, Col, NewL).

replace([], _, _, _).
replace([_|L], Elem, 0, [Elem|L]).
replace([X|L], Elem, Col, [X|N]) :-
    Col > 0,
    Col1 is Col - 1,
    replace(L, Elem, Col1, N).


:- dynamic choice/1.

getCode(Choice) :- 
    asserta((choice(10):-!)),    
    get_code(Code1),
    between(48, 57, Code1),
    Num1 is Code1 - 48,
    asserta((choice(Num1):-!)),
    peek_code(Code2),
    between(48, 57, Code2),
    Num2 is Code2 - 48,
    Choice is Num1 * 10 + Num2,
    skip_line, !,
    abolish(choice/1).

getCode(Choice) :-
    choice(Choice),
    Choice == 10, 
    abolish(choice/1),
    !, fail.

getCode(Choice) :-
    choice(Choice),
    skip_line,
    abolish(choice/1).