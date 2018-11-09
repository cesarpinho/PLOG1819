
casaVazia(0-0).

trocaJogador(1, 2).
trocaJogador(2, 1).

isEmpty([]).
isEmpty([_|_]) :- !, fail.

contaLinhas([X|L], NumL) :-  
    contaLinhas(L,N), 
    NumL is N + 1.

contaLinhas([],0).

tamanhoTabuleiro([H | T], Col, Lin) :-
    contaLinhas(H, Col),
    contaLinhas(T, X),
    Lin is X + 1.

espaco(1) :- write(" ").
espaco(N) :-
    N > 1,
    write(" "),
    Next is N - 1,
    espaco(Next).

novaLinha(1) :- nl.
novaLinha(N) :-
    N > 1,
    nl,
    Next is N - 1,
    novaLinha(Next).

not(X) :- X ,! ,fail.
not(X).

replace([X|L], Elem, 0, Col, [Y|NewL]) :-
    replace(X, Elem, Col, Y).
replace([X|L], Elem, Lin, Col, [X|NewL]) :-
    Lin > 0,
    Lin1 is Lin - 1,
    replace(L, Elem, Lin1, Col, NewL).

replace([], _, _, _).
replace([_|L], Elem, 0, [Elem|N]).
replace([X|L], Elem, Col, [X|N]) :-
    Col > 0,
    Col1 is Col - 1,
    replace(L, Elem, Col1, N). 


%%   Substituido pela operador NTH0()   %%
/* index( [JePeca|BoardList], JePeca, Num) :- Num is 0.
index([],_,_) :- !,fail.
index([X|BoardList], JePeca, Num) :-
    index(BoardList, JePeca, N),
    Num is N + 1. */