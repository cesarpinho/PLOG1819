menuJogo :-
    tabuleiro(Board),
    escolhaJogador(J),
    play(Board,J).

escolhaJogador(Escolha) :- 
    write("\n O jogador com as pecas pretas e' o primeiro a jogar.\n"),
    write(" Qual as pecas que prefere?\n"),
    write("   1 - Pretas\n"),
    write("   2 - Brancas\n"),
    write("   3 - Sair\n Escolha: "),
    get_code(Code),
    Escolha is Code - 48,
    validaEscolha(Escolha).

validaEscolha(1).
validaEscolha(2).
validaEscolha(3) :- !, fail.
validaEscolha(_) :-
    write("Erro: Escolha invalida.\n"),
    skip_line,
    menuJogo.

% TODO : menuVitoria

% TODO : menuGameOver(J, "Sem Movimentos").



     
    


    

    
    
