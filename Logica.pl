:- use_module(library(lists)).

play(Board, J) :-
    tamanhoTabuleiro(Board, Linhas, Colunas),
    imprimeJogo(Board, J, Linhas, Colunas),
    escolhePeca(J, Board, Peca),
    possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas),
    verificaQuantJogadas(J, Board, Jogadas),
    escolheJogada(J, Board, Linhas, Colunas, Jogadas, Jogada),
    efetuarJogada(J, Board, Peca, Linhas, Colunas, Jogadas, Jogada).


escolhePeca(J, Board, Peca) :-
    write("Escolha o numero da peca que pretende mover : "),
    get_code(Code),
    Escolha is Code - 48,    
    existePeca(J,Board, Escolha),
    Peca = Escolha.
escolhePeca(J, Board, _) :-
    write("Erro: Peca nao existente.\n"),
    skip_line,
    play(Board,J).

existePeca(J, Board, Escolha) :-
    append(Board, List),
    member(J-Escolha, List).

possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas) :-
    posicaoPeca(Board, J-Peca, Lin, Col, Colunas),
    Llimite is Linhas - 1,
    Climite is Colunas - 1,
    contaVizinhas(Board, Lin, Col, Llimite, Climite, NumV),
    movimentoHorizontal(J, Board, Lin, Col, Climite, NumV, MovH),
    movimentoVertical(J, Board, Lin, Col, Llimite, NumV, MovV),
    movimentoDiagonal(J, Board, Lin, Col, Llimite, Climite, NumV, MovD),
    append(MovH, MovV, MovHV),
    append(MovHV, MovD, Jogadas).

verificaQuantJogadas(J, Board, []) :-
    Peca = 1,
    verificaOutrasPecas(J, Board, Peca).
verificaQuantJogadas(J, Board, Jogadas).

verificaQuantJogadas(J, Board, Peca, []) :- 
    P is Peca + 1,
    verificaOutrasPecas(J, Board, Peca).
verificaQuantJogadas(J, Board, Peca, Jogadas).

verificaOutrasPecas(J, _, 13) :- !, menuGameOver(J, "Sem Movimentos"). 
verificaOutrasPecas(J, Board, Peca):-
    existePeca(J, Board, Peca),
    possiveisJogadas(J, Board, Peca, Jogadas),
    verificaQuantJogadas(J, Board, Peca, Jogadas).

escolheJogada(J, Board, Linhas, Colunas, Jogadas, Jogada) :-
    write(" Jogadas possiveis :\n"),
    imprimeJogada(Linhas, Jogadas, 1, TotalJog),
    novaLinha(1),
    write("Pressione 'V' para voltar...\n"),
    write("Escolha : "),
    get_char(Escolha), skip_line,
    ((Escolha == 'v' ; Escolha == 'V') -> play(Board,J); true),
    Escolha > 0,
    Escolha < TotalJog,
    nth1(Escolha, Jogadas, Jogada).
    
escolheJogada(J, Board, Linhas, Colunas, Jogadas, Jogada) :-
    write("Erro: Escolha invalida.\n\n"),
    skip_line,
    escolheJogada(J, Board, Linhas, Colunas, Jogadas, Jogada).

imprimeJogada(_, [], Indice, Indice).
imprimeJogada(QuantLin, [L-C-J|Jogadas], Indice, TotalJog) :-
    Lin is QuantLin - L,
    Col is C + 65,    
    espaco(2),
    write(Indice," -> "),
    put_code(Col),
    write(Lin),
    (J =\= 0 -> write(" *Captura* ") ; true),
    novaLinha(1),
    NextI is Indice + 1,
    imprimeJogada(QuantLin, Jogadas, NextI, TotalJog).
    
efetuarJogada(Jog, Board, Peca, Linhas, Colunas, Jogadas, L-C-J) :- 
    posicaoPeca(Board, Jog-Peca, LinP, ColP, Colunas),
    replace(Board, Jog-Peca, L, C, Board1),
    casaVazia(V),
    replace(Board1, V, LinP, ColP, NovoBoard),
    isCaptura(Jog, Board, Colunas, L-C-J),
    trocaJogador(Jog, NovoJ),
    play(NovoBoard, NovoJ).

isCaptura(Jog, Board, Colunas, L-C-J) :-
    J =\= 0,
    posicaoPeca(Board, Peca, L, C, Colunas),    
    write(" * Peca ", Peca, "capturada *\n"),
    asserta(captura(Jog,Peca)),
    (verificaVitoria(Jog) -> menuVitoria ; true).

verificaVitoria(Jog) :-
    pecasColhidas([], Pecas, Jog),
    sort(Pecas, PecasOrdenadas),
    verificaSequencia(PecasOrdenadas).
    
% TODO : verificaSequencia(PecasOrdenadas)
verificaSequencia(Pecas) :-
    verificaSequencia(Pecas, 0, UltimaP).

verificaSequencia([], _, _) :- fail.
verificaSequencia([X|Pecas], 0, UltimaP) :-
    UltimaP = X,
    verificaSequencia([Pecas], 1, UltimaP).

verificaSequencia([X|Pecas], Quant, UltimaP) :-
    NextPeca is UltimaP + 1,
    NextPeca == X,
    NextQ is Quant + 1,
    verificaSequencia([Pecas], NextQ, NextPeca).

verificaSequencia([X|Pecas], Quant, UltimaP) :-
    NextPeca is UltimaP + 1,
    NextPeca =\= X,
    NextQ is 0,
    verificaSequencia([Pecas], NextQ, X).    

pecasColhidas(L1,L,J) :- 
    captura(J-X),
    not(member(X, L1)),
    append(L1,[X],List),
    pecasColhidas(List,L,J). 

pecasColhidas(L,L,_).

retornaPeca(Board, Peca, L, C, Col) :-
    Num is (L * Col) + C,
    nth0(Num, BoardList, J-Peca).


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
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L > 0, C > 0, L < Llimite, C < Climite, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    posicaoOcupada(Board,L1,C3,Climite,N3),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N4),
    posicaoOcupada(Board,L2,C3,Climite,N5),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Climite,N6),
    posicaoOcupada(Board,L3,C2,Climite,N7),
    posicaoOcupada(Board,L3,C3,Climite,N8),
    NumV is N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8.

%        C1  C2 %
%  L1    P   1  %
%  L2    2   3  %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == 0, C == 0, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C2,Climite,N1),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N2),
    posicaoOcupada(Board,L2,C2,Climite,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   P   %
%  L2    2   3   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == 0 , C == Climite, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N2),
    posicaoOcupada(Board,L2,C2,Climite,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == Llimite , C == 0, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C2,Climite,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == Llimite , C == Climite, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N3),
    NumV is N1 + N2 + N3.

%        C1  C2  C3  %
%  L1    1   P   2   %
%  L2    3   4   5   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == 0, C > 0, C < Climite, !, 
    L1 is L, 
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C3,Climite,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N3),
    posicaoOcupada(Board,L2,C2,Climite,N4),
    posicaoOcupada(Board,L2,C3,Climite,N5),
    NumV is N1 + N2 + N3 + N4 + N5.

%        C1  C2  C3  %
%  L1    1   2   3   %
%  L2    4   P   5   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L == Llimite, C > 0, C < Climite, !, 
    L1 is L - 1, 
    L2 is L,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    posicaoOcupada(Board,L1,C3,Climite,N3),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N4),
    posicaoOcupada(Board,L2,C3,Climite,N5),
    NumV is N1 + N2 + N3 + N4 + N5.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
%  L3    4   5   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L > 0, L < Llimite, C == 0, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C2,Climite,N3),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Climite,N4),
    posicaoOcupada(Board,L3,C2,Climite,N5),
    NumV is N1 + N2 + N3 + N4 + N5.


%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
%  L3    4   5   %
contaVizinhas(Board, L, C, Llimite, Climite, NumV) :-
    L > 0, L < Llimite, C == Climite, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    posicaoOcupada(Board,L1,C1,Climite,N1),
    posicaoOcupada(Board,L1,C2,Climite,N2),
    %%  L2  %%
    posicaoOcupada(Board,L2,C1,Climite,N3),
    %%  L3  %%
    posicaoOcupada(Board,L3,C1,Climite,N4),
    posicaoOcupada(Board,L3,C2,Climite,N5),
    NumV is N1 + N2 + N3 + N4 + N5.


posicaoOcupada(Board, L, C, Climite, Ocupada) :-
    append(Board, BoardList),
    Col is Climite + 1,
    Num is (L * Col) + C,
    nth0(Num, BoardList, Peca),
    (casaVazia(Peca) -> Ocupada is 0; Ocupada is 1).

%%%---------------------------------------------------%%%
%%%     Calcula os movimentos possiveis de uma peca   %%% 
%%%---------------------------------------------------%%%
%%%     Movimento Horizontal     %%%
movimentoHorizontal(J, Board, LinP, ColP, Climite, NumV, MovH) :-
    nth0(LinP, Board, Linha),
    (movimentoDireita(J, LinP, ColP, Linha, Climite, NumV, MovHD) -> true ; MovHD = []),
    (movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE) -> true ; MovHE = []),
    append(MovHE,MovHD, MovH).

movimentoDireita(J, LinP, ColP, Linha, Climite, NumV, MovHD) :-
    NovaCol is ColP + NumV,
    NovaCol =< Climite,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovHD = [LinP-NovaCol-NovoJ] ; MovHD = []).

movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE) :-
    NovaCol is ColP - NumV,
    NovaCol >= 0,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovHE = [LinP-NovaCol-NovoJ] ; MovHE = []).

%%%       Movimento Vertical       %%%
movimentoVertical(J, Board, LinP, ColP, Llimite, NumV, MovV) :-
    (movimentoCima(J, Board, LinP, ColP, NumV, MovC) -> true ; MovC = [] ),
    (movimentoBaixo(J, Board, LinP, ColP, Llimite, NumV, MovB) -> true ; MovB = [] ),
    append(MovC, MovB, MovV).

movimentoCima(J, Board, LinP, ColP, NumV, MovC) :-
    NovaL is LinP - NumV,
    NovaL >= 0,
    nth0(NovaL, Board, NovaLinha),
    nth0(ColP, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovC = [NovaL-ColP-NovoJ] ; MovC = []).

movimentoBaixo(J, Board, LinP, ColP, Llimite, NumV, MovB) :-
    NovaL is LinP + NumV,
    NovaL =< Llimite,
    nth0(NovaL, Board, NovaLinha),
    nth0(ColP, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovB = [NovaL-ColP-NovoJ] ; MovB = []).

%%%       Movimento Diagonal       %%%
movimentoDiagonal(J, Board, LinP, ColP, Llimite, Climite, NumV, MovD) :- 
    (movimentoCimaEsquerda(J, Board, LinP, ColP, NumV, MovCE) -> true ; MovCE = []),
    (movimentoCimaDireita(J, Board, LinP, ColP, Climite, NumV, MovCD) -> true ; MovCD = []),
    (movimentoBaixoEsquerda(J, Board, LinP, ColP, Llimite, NumV, MovBE) -> true ; MovBE = []),
    (movimentoBaixoDireita(J, Board, LinP, ColP, Llimite, Climite, NumV, MovBD) -> true ; MovBD = []),
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

movimentoCimaDireita(J, Board, LinP, ColP, Climite, NumV, MovCD) :- 
    NovaL is LinP - NumV,
    NovaL >= 0,
    NovaC is ColP + NumV,
    NovaC =< Climite,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovCD = [NovaL-NovaC-NovoJ] ; MovCD = []).

movimentoBaixoEsquerda(J, Board, LinP, ColP, Llimite, NumV, MovBE) :-
    NovaL is LinP + NumV,
    NovaL =< Llimite,
    NovaC is ColP - NumV,
    NovaC >= 0,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovBE = [NovaL-NovaC-NovoJ] ; MovBE = []).

movimentoBaixoDireita(J, Board, LinP, ColP, Llimite, Climite, NumV, MovBD) :-
    NovaL is LinP + NumV,
    NovaL =< Llimite,
    NovaC is ColP + NumV,
    NovaC =< Climite,
    nth0(NovaL, Board, NovaLinha),
    nth0(NovaC, NovaLinha, NovoJ-NovaPeca),
    ((casaVazia(NovoJ-NovaPeca) ; NovoJ =\= J) -> MovBD = [NovaL-NovaC-NovoJ] ; MovBD = []).