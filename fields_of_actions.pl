:- include('Interface.pl').
:- include('Logica.pl').
:- include('Menus.pl').
:- include('Utilitarios.pl').

tabuleiro([ [1-8 ,1-7, 1-6 ,1-5, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0,1-12,1-11,1-10, 1-9],
            [1-4, 1-3, 1-2, 1-1, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 2-1, 2-2, 2-3, 2-4],
            [2-9,2-10,2-11,2-12, 0-0, 0-0, 0-0, 0-0],
            [0-0, 0-0, 0-0, 0-0, 2-5, 2-6, 2-7, 2-8]]).

fields_of_action :- /* tabuleiro(Board),
possiveisJogadasSemVizinhas(Board,1,4,8,8,1,NewB),
maplist(write, NewB),
proper_length(NewB, L),
novaLinha(1),
write(L) */

    menuJogo.

:- use_module(library(lists)).

 




captura(2-1).
captura(2-2).
captura(2-3).
captura(2-4).
captura(2-5).

/*
pecasColhidas(L1,L,J) :- captura(J-X), not(member(X, L1)) ,append(L1,[X],List), pecasColhidas(List,L,J). 
pecasColhidas(L,L,_).  */