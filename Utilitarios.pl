:- use_module(library(between)).

casaVazia(0-0).

clr :- write('\33\[2J').

trocaJogador(1, 2).
trocaJogador(2, 1).

isEmpty([]).
isEmpty([_|_]) :- !, fail.

contaLinhas([_|L], NumL) :-  
    contaLinhas(L,N), 
    NumL is N + 1.

contaLinhas([],0).

tamanhoTabuleiro([H | T], Col, Lin) :-
    contaLinhas(H, Col),
    contaLinhas(T, X),
    Lin is X + 1.

espaco(1) :- write(' ').
espaco(N) :-
    N > 1,
    write(' '),
    Next is N - 1,
    espaco(Next).

novaLinha(1) :- nl.
novaLinha(N) :-
    N > 1,
    nl,
    Next is N - 1,
    novaLinha(Next).

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

:- dynamic escolha/1.

getCode(Escolha) :-
    asserta((escolha(10):-!)),    
    get_code(Code1),
    between(48, 57, Code1),
    Num1 is Code1 - 48,
    asserta((escolha(Num1):-!)),
    peek_code(Code2),
    between(48, 57, Code2),
    Num2 is Code2 - 48,
    Escolha is Num1 * 10 + Num2,
    skip_line,
    abolish(escolha/1).

getCode(Escolha) :-
    escolha(Escolha),
    Escolha == 10, 
    abolish(escolha/1),
    !, fail.

getCode(Escolha) :-
    escolha(Escolha),
    skip_line,
    abolish(escolha/1).