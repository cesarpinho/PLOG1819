

contaLinhas([X|L], NumL) :-  
    contaLinhas(L,N), 
    NumL is N + 1.

contaLinhas([],0).

tamanhoTabuleiro([H | T], Col, Lin) :-
    contaLinhas(H, Col),
    contaLinhas(T, X),
    Lin is X + 1.

espaco(N) :-
    N > 0,
    write(' '),
    Next is N - 1,
    espaco(Next).

espaco(0).
    