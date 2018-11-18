:- use_module(library(lists)).
:- dynamic catch/1.

play(Board, J, _, _) :-
    is_game_over(GameOver), GameOver == true,
    abolish(is_game_over/1).

play(Board, J, human, Type) :-
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Linhas, Colunas),
    display_game(Board, J, Linhas, Colunas), repeat,
    escolhePeca(J, Board, Peca),
    possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas),
    verificaQuantJogadas(J, Board, Peca, Jogadas),
    escolheJogada(J, Board, human, Type, Linhas, Jogadas, Jogada),
    efetuarJogada(J, Board, Peca, Colunas, Jogada, NovoBoard),
    change_player(J, NovoJ),
    play(NovoBoard, NovoJ, Type, human).

play(Board, J, computer1, Type) :- 
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Linhas, Colunas),
    display_game(Board, J, Linhas, Colunas),
    valid_moves(Board, J, Linhas, Colunas, ListOfMoves),
    choose_move(Board, J, computer1, ListOfMoves, Move),
    move(J, Board, Linhas, Colunas, Move, NovoBoard),
    wait_enter,
    change_player(J, NewJ),
    play(NovoBoard, NewJ, Type, computer1).

play(Board, J, computer2, Type) :-
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Linhas, Colunas),
    display_game(Board, J, Linhas, Colunas),
    valid_moves(Board, J, Linhas, Colunas, ListOfMoves),
    choose_move(Board, J, computer2, ListOfMoves, Move),
    move(J, Board, Linhas, Colunas, Move, NovoBoard),
    wait_enter,
    change_player(J, NewJ),
    play(NovoBoard, NewJ, Type, computer2).

escolhePeca(J, Board, Peca) :-
    write('Escolha o numero da peca que pretende mover : '),
    getCode(Choice),
    new_line(1),
    existePeca(J,Board, Choice), 
    Peca = Choice.
escolhePeca(J, Board, _) :-
    write('Erro: Peca nao existente.'),
    new_line(2),
    !,fail.

existePeca(J, Board, Choice) :-
    append(Board, List),
    member(J-Choice, List).

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

filtraJogadas(Board, Linhas, Colunas, [L-C-NewL-NewC-J|Jogadas], [X|List]) :- 
    contaVizinhas(Board, NewL, NewC, Linhas, Colunas, NumV),
    NumV >= 2,
    J =:= 0,
    X = L-C-NewL-NewC-J,
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

verificaOutrasPecas(J, _, 13) :- !, game_over(J). 
verificaOutrasPecas(J, Board, Peca):-
    existePeca(J, Board, Peca),
    board_size(Board, Linhas, Colunas),
    possiveisJogadas(J, Board, Peca, Linhas, Colunas, Jogadas),
    verificaQuantJogadas(J, Board, Peca, Jogadas).

escolheJogada(J, Board, Type1, Type2, Linhas, Jogadas, Jogada) :-
    write(' Jogadas possiveis :\n'),
    imprimeJogada(Linhas, Jogadas, 1, TotalJog),
    new_line(1),
    space(2),
    write('0 -> Voltar\n'),
    write('Escolha : '),
    peek_code(Code),
    Code =\= 48,
    getCode(Choice),
    Choice > 0,
    Choice < TotalJog, 
    nth1(Choice, Jogadas, Jogada), !.

escolheJogada(J, Board, Type1, Type2, Linhas, Jogadas, Jogada) :-
    getCode(Code),
    Code =:= 48, !,
    play(Board, J, Type1, Type2).

escolheJogada(J, Board, Type1, Type2, Linhas, Jogadas, Jogada) :-
    write('Erro: Escolha invalida.\n\n'),
    escolheJogada(J, Board, Type1, Type2, Linhas, Jogadas, Jogada).

imprimeJogada(_, [], Indice, Indice).
imprimeJogada(QuantLin, [_-_-NewL-NewC-J|Jogadas], Indice, TotalJog) :-
    Lin is QuantLin - NewL,
    Col is NewC + 65,
    space(2),
    write(Indice),
    write(' -> '),
    put_code(Col),
    write(Lin),
    (J =\= 0 -> write(' *Captura* ') ; true),
    new_line(1),
    NextI is Indice + 1,
    imprimeJogada(QuantLin, Jogadas, NextI, TotalJog).
    
efetuarJogada(Jog, Board, Peca, Colunas, L-C-NewL-NewC-J, NovoBoard) :- 
    replace(Board, Jog-Peca, NewL, NewC, Board1),
    empty_cel(V),
    replace(Board1, V, L, C, NovoBoard),
    is_catch(Jog, Board, Colunas, L-C-NewL-NewC-J).

is_catch(_, _, _, _-_-_-_-0).
is_catch(Jog, Board, Colunas, L-C-NewL-NewC-J) :-
    J =\= 0, J =\= Jog,
    posicaoPeca(Board, J-Peca, NewL, NewC, Colunas),    
    write(' * Peca '),
    write(Peca),
    write(' capturada *\n'),
    asserta(catch(Jog-Peca)),
    check_vitory(Jog).

check_vitory(Jog) :-
    pecasCapturadas(Jog, Pecas),
    sort(Pecas, PecasOrdenadas),
    check_sequence(PecasOrdenadas, _, Value),
    Value >= 5,
    game_over(Jog, PecasOrdenadas).

check_vitory(_).

check_sequence([X|Pecas], FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    check_sequence([X|Pecas], 1, X-X, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq).

check_sequence([], _, _, 0-0, 0).
check_sequence([X|Pecas], Quant, FirstP-LastP, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    NextPeca is LastP + 1,
    NextPeca =:= X, 
    NextQ is Quant + 1,
    check_sequence(Pecas, NextQ, FirstP-X, FirstSeqPiece-LastSeqPiece, Seq),
    save_sequence(NextQ, FirstP-X, FirstSeqPiece-LastSeqPiece, Seq, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq).

check_sequence([X|Pecas], Quant, FirstP-LastP, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    NextPeca is LastP + 1,
    NextPeca =\= X,
    NextQ is 1,
    check_sequence(Pecas, NextQ, X-X, FirstSeqPiece-LastSeqPiece, Seq),
    save_sequence(NextQ, X-X, FirstSeqPiece-LastSeqPiece, Seq, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq).

save_sequence(Quant, FirstP-LastP, OldFirstPiece-OldLastPiece, SeqOrder, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    Quant > SeqOrder,
    GreaterSeq = Quant,
    FirstGreatSeqPiece = FirstP,
    LastGreatSeqPiece = LastP.
save_sequence(_, _, OldFirstPiece-OldLastPiece, SeqOrder, OldFirstPiece-OldLastPiece, SeqOrder).

pecasCapturadas(Jog, PecasCapturadas) :-
    pecasCapturadas([], PecasCapturadas, Jog).

pecasCapturadas(L1,L,J) :- 
    catch(J-X),
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
    C is mod(Num, Col),!.

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
    empty_cel(Peca),
    Ocupada is 0.
posicaoOcupada(_, _, _, _, 1).

%%%---------------------------------------------------%%%
%%%     Calcula os movimentos possiveis de uma peca   %%% 
%%%---------------------------------------------------%%%
%%%     Movimento Horizontal     %%%
movimentoHorizontal(J, Board, LinP, ColP, Colunas, NumV, MovH) :-
    nth0(LinP, Board, Linha),
    movimentoDireita(J, LinP, ColP, Linha, Colunas, NumV, MovHD),
    movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE),
    append(MovHE,MovHD, MovH).

movimentoDireita(J, LinP, ColP, Linha, Colunas, NumV, MovHD) :-
    NovaCol is ColP + NumV,
    NovaCol < Colunas,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovHD = [LinP-ColP-LinP-NovaCol-NovoJ].
movimentoDireita(_, _, _, _, _, _, []).

movimentoEsquerda(J, LinP, ColP, Linha, NumV, MovHE) :-
    NovaCol is ColP - NumV,
    NovaCol >= 0,
    nth0(NovaCol, Linha, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovHE = [LinP-ColP-LinP-NovaCol-NovoJ].
movimentoEsquerda(_, _, _, _, _, []).

%%%       Movimento Vertical       %%%
movimentoVertical(J, Board, LinP, ColP, Linhas, NumV, MovV) :-
    movimentoCima(J, Board, LinP, ColP, NumV, MovC),
    movimentoBaixo(J, Board, LinP, ColP, Linhas, NumV, MovB),
    append(MovC, MovB, MovV).

movimentoCima(J, Board, LinP, ColP, NumV, MovC) :-
    NovaL is LinP - NumV,
    NovaL >= 0,
    nth0(NovaL, Board, new_line),
    nth0(ColP, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovC = [LinP-ColP-NovaL-ColP-NovoJ].
movimentoCima(_, _, _, _, _, []).

movimentoBaixo(J, Board, LinP, ColP, Linhas, NumV, MovB) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    nth0(NovaL, Board, new_line),
    nth0(ColP, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovB = [LinP-ColP-NovaL-ColP-NovoJ].
movimentoBaixo(_, _, _, _, _, _, []).

%%%       Movimento Diagonal       %%%
movimentoDiagonal(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovD) :- 
    movimentoCimaEsquerda(J, Board, LinP, ColP, NumV, MovCE),
    movimentoCimaDireita(J, Board, LinP, ColP, Colunas, NumV, MovCD),
    movimentoBaixoEsquerda(J, Board, LinP, ColP, Linhas, NumV, MovBE),
    movimentoBaixoDireita(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovBD),
    append(MovCE, MovCD, Mov1),
    append(Mov1, MovBE, Mov2),
    append(Mov2, MovBD, MovD).

movimentoCimaEsquerda(J, Board, LinP, ColP, NumV, MovCE) :- 
    NovaL is LinP - NumV,
    NovaL >= 0,
    NovaC is ColP - NumV,
    NovaC >= 0,
    nth0(NovaL, Board, new_line),
    nth0(NovaC, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovCE = [LinP-ColP-NovaL-NovaC-NovoJ].
movimentoCimaEsquerda(_, _, _, _, _, []).

movimentoCimaDireita(J, Board, LinP, ColP, Colunas, NumV, MovCD) :- 
    NovaL is LinP - NumV,
    NovaL >= 0,
    NovaC is ColP + NumV,
    NovaC < Colunas,
    nth0(NovaL, Board, new_line),
    nth0(NovaC, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovCD = [LinP-ColP-NovaL-NovaC-NovoJ].
movimentoCimaDireita(_, _, _, _, _, _, []).

movimentoBaixoEsquerda(J, Board, LinP, ColP, Linhas, NumV, MovBE) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    NovaC is ColP - NumV,
    NovaC >= 0,
    nth0(NovaL, Board, new_line),
    nth0(NovaC, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovBE = [LinP-ColP-NovaL-NovaC-NovoJ].
movimentoBaixoEsquerda(_, _, _, _, _, _, []).

movimentoBaixoDireita(J, Board, LinP, ColP, Linhas, Colunas, NumV, MovBD) :-
    NovaL is LinP + NumV,
    NovaL < Linhas,
    NovaC is ColP + NumV,
    NovaC < Colunas,
    nth0(NovaL, Board, new_line),
    nth0(NovaC, new_line, NovoJ-NovaPeca),
    (empty_cel(NovoJ-NovaPeca) ; NovoJ =\= J),
    MovBD = [LinP-ColP-NovaL-NovaC-NovoJ].
movimentoBaixoDireita(_, _, _, _, _, _, _, []).