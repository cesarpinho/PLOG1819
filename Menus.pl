
% TODO : menuInicial % escolher o tipo de jogo (J-J, J-C, C-C)

menuJogo :-
    tabuleiro(Board),
    escolhaJogador(J),
    play(Board,J).

escolhaJogador(Escolha) :- 
    write('\n O jogador com as pecas pretas e o primeiro a jogar.\n'),
    write(' Qual as pecas que prefere?\n'),
    write('   1 - Pretas\n'),
    write('   2 - Brancas\n'),
    write('   3 - Sair\n Escolha: '),
    getCode(Escolha),
    validaEscolhaJogador(Escolha).

validaEscolhaJogador(1).
validaEscolhaJogador(2).
validaEscolhaJogador(3) :- !, fail.
validaEscolhaJogador(_) :-
    write('Erro: Escolha invalida.\n'),
    skip_line,
    menuJogo.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
game_over(+Board, -Winner).


menuVitoria(Jogador, Pecas) :-
    abolish(captura/1),

    novaLinha(1),
    write('*********************************************\n'),
    write('**          VENCEDOR :: JOGADOR '),
    write(Jogador),
    write('          **\n'),
    write('*********************************************\n'),
    write('  PECAS CAPTURADAS :\n'),
    imprimePecasCapturadas(Jogador,Pecas), break
    .

imprimePecasCapturadas(1, [P|Pecas]) :-
    espaco(3),
    imprimeJogador(2-_),
    imprimePeca(_-P),
    novaLinha(1),
    imprimePecasCapturadas(1, Pecas).
imprimePecasCapturadas(2, [P|Pecas]) :-
    espaco(3),
    imprimeJogador(1-_),
    imprimePeca(_-P),
    novaLinha(1),
    imprimePecasCapturadas(2, Pecas).
imprimePecasCapturadas(_, []).


menuGameOver(1) :-
    write('\nJOGADOR '),
    write(1),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    pecasCapturadas([], Pecas, 2),
    sort(Pecas, PecasOrdenadas),
    menuVitoria(2, PecasOrdenadas).

menuGameOver(2) :-
    write('\nJOGADOR '),
    write(2),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    pecasCapturadas([], Pecas, 2),
    sort(Pecas, PecasOrdenadas),
    menuVitoria(1, PecasOrdenadas).

    



     
    


    

    
    
