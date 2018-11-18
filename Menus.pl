:- dynamic is_game_over/1.

% TODO : menuInicial % escolher o tipo de jogo (J-J, J-C, C-C)

game_menu :-
    asserta(is_game_over(false)),
    board(Board),
    choose_player(J),
    play_game(Board, J).

play_game(Board, 1) :- play(Board,1, human, computer2).
play_game(Board, 2) :- play(Board,1, computer2, computer2).
play_game(Board, 3).

choose_player(Choice) :- 
    write('\n O jogador com as pecas pretas e o primeiro a jogar.\n'),
    write(' Qual as pecas que prefere?\n'),
    write('   1 - Pretas\n'),
    write('   2 - Brancas\n'),
    write('   3 - Sair\n Escolha: '),
    getCode(Choice),
    check_choice_player(Choice).

check_choice_player(1).
check_choice_player(2).
check_choice_player(3).
check_choice_player(_) :-
    write('Erro: Escolha invalida.\n'),
    game_menu.

game_over(Player, Pieces) :-
    abolish(catch/1),

    new_line(1),
    write('*********************************************\n'),
    write('**          VENCEDOR :: JOGADOR '), write(Player), write('          **\n'),
    write('*********************************************\n'),
    write('  PECAS CAPTURADAS :\n'),
    display_catched_pieces(Player,Pieces),
    asserta(is_game_over(true)).

display_catched_pieces(1, [P|Pieces]) :-
    space(3),
    display_player(2-_),
    display_piece(_-P),
    new_line(1),
    display_catched_pieces(1, Pieces).
display_catched_pieces(2, [P|Pieces]) :-
    space(3),
    display_player(1-_),
    display_piece(_-P),
    new_line(1),
    display_catched_pieces(2, Pieces).
display_catched_pieces(_, []).


game_over(1) :-
    write('\nJOGADOR '),
    write(1),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    pecasCapturadas([], Pieces, 2),
    sort(Pieces, OrderedPieces),
    game_over(2, OrderedPieces).

game_over(2) :-
    write('\nJOGADOR '),
    write(2),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    pecasCapturadas([], Pieces, 2),
    sort(Pieces, OrderedPieces),
    game_over(1, OrderedPieces).

    



     
    


    

    
    
