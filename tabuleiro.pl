
tabuleiro([ [1-8 ,1-7, 1-6 ,1-5, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0,1-12,1-11,1-10, 1-9],
            [1-4, 1-3, 1-2, 1-1, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 2-1, 2-2, 2-3, 2-4],
            [2-9,2-10,2-11,2-12, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 2-5, 2-6, 2-7, 2-8]]).

display_game(Board, Player) :-
    tamanhoTabuleiro(Board, Col, Lin),
    nl,
    imprimeNCol(Col),
    nl,
    imprimeTab(Board, Lin, Col),
    imprimeNCol(Col),
    nl.


imprimeNCol(N) :-
    espaco(6),
    Letra is 65,
    put_code(Letra),    
    espaco(2),
    N1 is N - 1,
    Next is Letra + 1,
    imprimeNCol(Next, N1).

imprimeNCol(_,0).
imprimeNCol(Letra, N) :-
    espaco(2),
    put_code(Letra),
    espaco(2),
    N1 is N - 1,
    Next is Letra + 1,
    imprimeNCol(Next, N1).

imprimeNumLin(N) :-
    write(N).


imprimeSeparLin(Col) :-
    espaco(3),
    write('------'),
    N1 is Col - 1,
    imprimeSeparLin(N1, Col).

imprimeSeparLin(N, Col) :-
    N > 0, !,
    write('-----'),
    N1 is N - 1,
    imprimeSeparLin(N1, Col).

imprimeSeparLin(0, _) :- nl.

imprimeSeparCol :-
    write(' | ').

imprimeNumLin_e_SeparCol(NumL) :-
    NumL > 0,
    espaco(1),
    imprimeNumLin(NumL),
    imprimeSeparCol.

imprimeNumLin_e_SeparCol(0) :-
    espaco(2),
    imprimeSeparCol.

imprimeTab([L|T], NumL, Col) :-
    imprimeSeparLin(Col),
    imprimeNumLin_e_SeparCol(NumL),

    imprimeLinhaJogador(L),

    imprimeNumLin(NumL), nl,
    imprimeNumLin_e_SeparCol(0),

    imprimeLinhaPeca(L),

    nl,
    NextL is NumL - 1,
    imprimeTab(T, NextL, Col).

imprimeTab([], _, Col) :- imprimeSeparLin(Col).


imprimeLinhaJogador([]).

imprimeLinhaJogador([C|L]) :-   
    imprimeJogador(C),
    imprimeSeparCol,
    imprimeLinhaJogador(L).

imprimeLinhaPeca([]).

imprimeLinhaPeca([C|L]) :-
    imprimePeca(C),
    imprimeSeparCol,
    imprimeLinhaPeca(L).

imprimeJogador(0-0) :- espaco(2).
imprimeJogador(1-_) :- write('B'), espaco(1).
imprimeJogador(2-_) :- write('W'), espaco(1).

imprimePeca(0-0) :- espaco(2).
imprimePeca(_-X) :- X < 10, espaco(1), write(X).
imprimePeca(_-X) :- X >= 10, write(X).