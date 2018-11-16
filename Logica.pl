:- use_module(library(lists)).
:- dynamic captura/1.

play(Board, J) :-
    tamanhoTabuleiro(Board, Linhas, Colunas),
    imprimeJogo(Board, J, Linhas, Colunas),
    escolhePeca(J, Board, Peca),
    possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas),
    verificaQuantJogadas(J, Board, Peca, Jogadas),
    escolheJogada(J, Board, Linhas, Jogadas, Jogada),
    efetuarJogada(J, Board, Peca, Colunas, Jogada).


escolhePeca(J, Board, Peca) :-
    write('Escolha o numero da peca que pretende mover : '),
    getCode(Escolha),   
    existePeca(J,Board, Escolha), !,
    Peca = Escolha.
escolhePeca(J, Board, _) :-
    write('Erro: Peca nao existente.\n'),
    play(Board,J).

existePeca(J, Board, Escolha) :-
    append(Board, List),
    member(J-Escolha, List).

possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas) :-
    posicaoPeca(Board, J-Peca, Lin, Col, Colunas),
    contaVizinhas(Board, Lin, Col, Linhas, Colunas, NumV),
    NumV =\= 0, !,
    movimentoHorizontal(J, Board, Lin, Col, Colunas, NumV, MovH),
    movimentoVertical(J, Board, Lin, Col, Linhas, NumV, MovV),
    movimentoDiagonal(J, Board, Lin, Col, Linhas, Colunas, NumV, MovD),
    append(MovH, MovV, MovHV),
    append(MovHV, MovD, Jogadas).

possiveisJogadas(J, Board, Peca, Linhas, Colunas, JogadasValidas) :-
    posicaoPeca(Board, J-Peca, Lin, Col, Colunas),
    possiveisJogadasSemVizinhas(J, Board, Lin, Col, Linhas, Colunas, 1, Jogadas),
    replace(Board, 0-0, Lin, Col, Board1),
    filtraJogadas(Board1, Linhas, Colunas, Jogadas, JogadasValidas).


possiveisJogadasSemVizinhas(J, Board, Lin, Col, Linhas, Colunas, NumV, Jogadas) :-
    NumV < Linhas, 
    NumV < Colunas,
    movimentoHorizontal(J, Board, Lin, Col, Colunas, NumV, MovH),
    movimentoVertical(J, Board, Lin, Col, Linhas, NumV, MovV),
    movimentoDiagonal(J, Board, Lin, Col, Linhas, Colunas, NumV, MovD),
    NovoNumV is NumV + 1,
    possiveisJogadasSemVizinhas(J, Board, Lin, Col, Linhas, Colunas, NovoNumV, Jogadas1),
    append(MovH, MovV, MovHV),
    append(MovHV, MovD, NJogadas),
    append(NJogadas, Jogadas1, Jogadas).

possiveisJogadasSemVizinhas(_, _, _, _, _, _, _, []). 

filtraJogadas(Board, Linhas, Colunas, [L-C-J|Jogadas], [X|List]) :- 
    contaVizinhas(Board, L, C, Linhas, Colunas, NumV),
    NumV >= 2,
    J =:= 0,
    X = L-C-J,
    filtraJogadas(Board, Linhas, Colunas, Jogadas, List).

filtraJogadas(Board, Linhas, Colunas, [_|Jogadas], List) :- 
    filtraJogadas(Board, Linhas, Colunas, Jogadas, List).

filtraJogadas(_, _, _, [], []).


verificaQuantJogadas(J, Board, []) :-
    Peca = 1,
    verificaOutrasPecas(J, Board, Peca).
verificaQuantJogadas(_, _, _).

verificaQuantJogadas(J, Board, Peca, []) :-
    P is Peca + 1,
    verificaOutrasPecas(J, Board, P),
    write('A peca escolhida nao tem movimentos possiveis. \n'),
    !, fail.
verificaQuantJogadas(_, _, _, L) :- tail(L, _).

verificaOutrasPecas(J, _, 13) :- !, menuGameOver(J, 'Sem Movimentos'). 
verificaOutrasPecas(J, Board, Peca):-
    existePeca(J, Board, Peca),
    tamanhoTabuleiro(Board, Linhas, Colunas),
    possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas),
    verificaQuantJogadas(J, Board, Peca, Jogadas).

escolheJogada(J, Board, Linhas, Jogadas, Jogada) :-
    write(' Jogadas possiveis :\n'),
    imprimeJogada(Linhas, Jogadas, 1, TotalJog),
    novaLinha(1),
    espaco(2),
    write('0 -> Voltar\n'),
    write('Escolha : '),
    getCode(Escolha),
    (Escolha =:= 0 -> play(Board,J) ; true),
    Escolha > 0,
    Escolha < TotalJog, 
    nth1(Escolha, Jogadas, Jogada), !.
    
escolheJogada(J, Board, Linhas, Jogadas, Jogada) :-
    write('Erro: Escolha invalida.\n\n'),
    escolheJogada(J, Board, Linhas, Jogadas, Jogada).

imprimeJogada(_, [], Indice, Indice).
imprimeJogada(QuantLin, [L-C-J|Jogadas], Indice, TotalJog) :-
    Lin is QuantLin - L,
    Col is C + 65,    
    espaco(2),
    write(Indice),
    write(' -> '),
    put_code(Col),
    write(Lin),
    (J =\= 0 -> write(' *Captura* ') ; true),
    novaLinha(1),
    NextI is Indice + 1,
    imprimeJogada(QuantLin, Jogadas, NextI, TotalJog).
    
efetuarJogada(Jog, Board, Peca, Colunas, L-C-J) :- 
    posicaoPeca(Board, Jog-Peca, LinP, ColP, Colunas),
    replace(Board, Jog-Peca, L, C, Board1),
    casaVazia(V),
    replace(Board1, V, LinP, ColP, NovoBoard),
    isCaptura(Jog, Board, Colunas, L-C-J),
    trocaJogador(Jog, NovoJ),
    play(NovoBoard, NovoJ).

isCaptura(_, _, _, _-_-0).
isCaptura(Jog, Board, Colunas, L-C-J) :-
    posicaoPeca(Board, J-Peca, L, C, Colunas),    
    write(' * Peca '),
    write(Peca),
    write(' capturada *\n'),
    asserta(captura(Jog-Peca)),
    verificaVitoria(Jog).

verificaVitoria(Jog) :-
    pecasCapturadas([], Pecas, Jog),
    sort(Pecas, PecasOrdenadas),
    verificaSequencia(PecasOrdenadas), !,
    menuVitoria(Jog, PecasOrdenadas).
verificaVitoria(_).

verificaSequencia(Pecas) :-
    verificaSequencia(Pecas, 0, _).

verificaSequencia(_, 5, _).
verificaSequencia([], _, _) :- fail.
verificaSequencia([X|Pecas], 0, UltimaP) :-
    UltimaP = X, !,
    verificaSequencia(Pecas, 1, UltimaP).

verificaSequencia([X|Pecas], Quant, UltimaP) :-
    NextPeca is UltimaP + 1,
    NextPeca =:= X, 
    NextQ is Quant + 1, !,
    verificaSequencia(Pecas, NextQ, NextPeca).

verificaSequencia([X|Pecas], _, UltimaP) :-
    NextPeca is UltimaP + 1,
    NextPeca =\= X,
    NextQ is 1, !,
    verificaSequencia(Pecas, NextQ, X).    

pecasCapturadas(L1,L,J) :- 
    captura(J-X),
    not(member(X, L1)),
    append(L1,[X],List),
    pecasCapturadas(List,L,J). 

pecasCapturadas(L,L,_).

%%%-----------------------------------------------------%%%
%%%   Calcula o numero de pecas adjacentes a uma peca   %%% 
%%%-----------------------------------------------------%%%
posicaoPeca(Board, JePeca, L, C, Col) :-
    append(Board, BoardList),
    nth0(Num, BoardList, JePeca),
    L is div(Num, Col),
    C is mod(Num, Col).

%        C1  C2  C3  %
%  L1    1   2   3   %
%  L2    4   P   5   %
%  L3    6   7   8   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L > 0, C > 0, L < Linhas-1, C < Colunas-1, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    posicaoOcupada(Board,L1,C3,Colunas,N3),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N4),
    posicaoOcupada(Board,L2,C3,Colunas,N5),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Colunas,N6),
    posicaoOcupada(Board,L3,C2,Colunas,N7),
    posicaoOcupada(Board,L3,C3,Colunas,N8),
    NumV is N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8.

%        C1  C2 %
%  L1    P   1  %
%  L2    2   3  %
contaVizinhas(Board, L, C, _, Colunas, NumV) :-
    L =:= 0, C =:= 0, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C2,Colunas,N1),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N2),
    posicaoOcupada(Board,L2,C2,Colunas,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   P   %
%  L2    2   3   %
contaVizinhas(Board, L, C, _, Colunas, NumV) :-
    L =:= 0 , C =:= Colunas-1, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N2),
    posicaoOcupada(Board,L2,C2,Colunas,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L =:= Linhas-1 , C =:= 0, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C2,Colunas,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L =:= Linhas-1 , C =:= Colunas-1, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  C3  %
%  L1    1   P   2   %
%  L2    3   4   5   %
contaVizinhas(Board, L, C, _, Colunas, NumV) :-
    L =:= 0, C > 0, C < Colunas-1, !, 
    L1 is L, 
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C3,Colunas,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N3),
    posicaoOcupada(Board,L2,C2,Colunas,N4),
    posicaoOcupada(Board,L2,C3,Colunas,N5),
    NumV is N1 + N2 + N3 + N4 + N5.

%        C1  C2  C3  %
%  L1    1   2   3   %
%  L2    4   P   5   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L =:= Linhas-1, C > 0, C < Colunas-1, !, 
    L1 is L - 1, 
    L2 is L,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    posicaoOcupada(Board,L1,C3,Colunas,N3),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N4),
    posicaoOcupada(Board,L2,C3,Colunas,N5),
    NumV is N1 + N2 + N3 + N4 + N5.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
%  L3    4   5   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L > 0, L < Linhas-1, C =:= 0, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C2,Colunas,N3),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Colunas,N4),
    posicaoOcupada(Board,L3,C2,Colunas,N5),
    NumV is N1 + N2 + N3 + N4 + N5.


%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
%  L3    4   5   %
contaVizinhas(Board, L, C, Linhas, Colunas, NumV) :-
    L > 0, L < Linhas-1, C =:= Colunas-1, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Colunas,N1),
    posicaoOcupada(Board,L1,C2,Colunas,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Colunas,N3),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Colunas,N4),
    posicaoOcupada(Board,L3,C2,Colunas,N5),
    NumV is N1 + N2 + N3 + N4 + N5.


posicaoOcupada(Board, L, C, Colunas, Ocupada) :-
    append(Board, BoardList),
    Num is (L * Colunas) + C,
    nth0(Num, BoardList, Peca),
    (casaVazia(Peca) -> Ocupada is 0; Ocupada is 1).

%%%---------------------------------------------------%%%
%%%     Calcula os movimentos possiveis de uma peca   %%% 
%%%---------------------------------------------------%%%
%%%     Movimento Horizontal     %%%
movimentoHorizontal(J, Board, LinP, ColP, Colunas, NumV, MovH) :-
    nth0(LinP, Board, Linha),
    (movimentoDireita(J, LinP, ColP, Linha, Colunas, NumV, MovHD) -> true ; MovHD = []),
    (movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE) -> true ; MovHE = []),
    append(MovHE,MovHD, MovH).

movimentoDireita(J, LinP, ColP, Linha, Colunas, NumV, MovHD) :-
    NovaCol is ColP + NumV,
    NovaCol < Colunas,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovHD = [LinP-NovaCol-NovoJ] ; MovHD = []).

movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE) :-
    NovaCol is ColP - NumV,
    NovaCol >= 0,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovHE = [LinP-NovaCol-NovoJ] ; MovHE = []).

%%%       Movimento Vertical       %%%
movimentoVertical(J, Board, LinP, ColP, Linhas, NumV, MovV) :-
    (movimentoCima(J, Board, LinP, ColP, NumV, MovC) -> true ; MovC = [] ),
    (movimentoBaixo(J, Board, LinP, ColP, Linhas, NumV, MovB) -> true ; MovB = [] ),
    append(MovC, MovB, MovV).

movimentoCima(J, Board, LinP, ColP, NumV, MovC) :-
    NovaL is LinP - NumV,
    NovaL >= 0,
    nth0(NovaL, Board, NovaLinha),
    nth0(ColP, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovC = [NovaL-ColP-NovoJ] ; MovC = []).

movimentoBaixo(J, Board, LinP, ColP, Linhas, NumV, MovB) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    nth0(NovaL, Board, NovaLinha),
    nth0(ColP, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovB = [NovaL-ColP-NovoJ] ; MovB = []).

%%%       Movimento Diagonal       %%%
movimentoDiagonal(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovD) :- 
    (movimentoCimaEsquerda(J, Board, LinP, ColP, NumV, MovCE) -> true ; MovCE = []),
    (movimentoCimaDireita(J, Board, LinP, ColP, Colunas, NumV, MovCD) -> true ; MovCD = []),
    (movimentoBaixoEsquerda(J, Board, LinP, ColP, Linhas, NumV, MovBE) -> true ; MovBE = []),
    (movimentoBaixoDireita(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovBD) -> true ; MovBD = []),
    append(MovCE, MovCD, Mov1),
    append(Mov1, MovBE, Mov2),
    append(Mov2, MovBD, MovD).

movimentoCimaEsquerda(J, Board, LinP, ColP, NumV, MovCE) :- 
    NovaL is LinP - NumV,
    NovaL >= 0,
    NovaC is ColP - NumV,
    NovaC >= 0,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovCE = [NovaL-NovaC-NovoJ] ; MovCE = []).

movimentoCimaDireita(J, Board, LinP, ColP, Colunas, NumV, MovCD) :- 
    NovaL is LinP - NumV,
    NovaL >= 0,
    NovaC is ColP + NumV,
    NovaC < Colunas,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovCD = [NovaL-NovaC-NovoJ] ; MovCD = []).

movimentoBaixoEsquerda(J, Board, LinP, ColP, Linhas, NumV, MovBE) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    NovaC is ColP - NumV,
    NovaC >= 0,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovBE = [NovaL-NovaC-NovoJ] ; MovBE = []).

movimentoBaixoDireita(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovBD) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    NovaC is ColP + NumV,
    NovaC < Colunas,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovBD = [NovaL-NovaC-NovoJ] ; MovBD = []).