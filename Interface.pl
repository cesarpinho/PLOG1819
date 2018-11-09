imprimeJogo(Board, Jogador, Lin, Col) :-
    novaLinha(1),
    imprimeNCol(Col),
    novaLinha(1),
    imprimeTab(Board, Lin, Col),
    imprimeNCol(Col),
    novaLinha(1),
    write('--> Jogador ',Jogador),
    novaLinha(1).

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
    write("------"),
    N1 is Col - 1,
    imprimeSeparLin(N1, Col).

imprimeSeparLin(N, Col) :-
    N > 0, !,
    write("-----"),
    N1 is N - 1,
    imprimeSeparLin(N1, Col).

imprimeSeparLin(0, _) :- nl.

imprimeSeparCol :-
    write(" | ").

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

    novaLinha(1),
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
imprimeJogador(1-_) :- write("B"), espaco(1).
imprimeJogador(2-_) :- write("W"), espaco(1).

imprimePeca(0-0) :- espaco(2).
imprimePeca(_-X) :- X < 10, espaco(1), write(X).
imprimePeca(_-X) :- X >= 10, write(X).