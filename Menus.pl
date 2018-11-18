display_header :-
    write('*******************************************************\n'),
    write('****                                               ****\n'),
    write('****                FIELDS OF ACTION               ****\n'),
    write('****                                               ****\n'),
    write('*******************************************************\n').

display_start_menu :-
    display_header,
    write('****                                               ****\n'),
    write('****                                               ****\n'),
    write('****                    1 - Jogar                  ****\n'),
    write('****                    2 - About                  ****\n'),
    write('****                    3 - Sair                   ****\n'),
    write('****                                               ****\n'),
    write('****                                               ****\n'),
    write('*******************************************************\n').

start_menu :- 
    clr,
    display_start_menu,
    get_number(Choice),
    (
        Choice =:= 1 -> choose_game_menu;
        Choice =:= 2 -> about_menu;
        Choice =:= 3;
        
        start_menu  
    ).

display_about_menu :-
    display_header,
    write('****                                               ****\n'),
    write('****                    Autores:                   ****\n'),
	write('****                - Angelo Moura                 ****\n'),
	write('****                - Cesar Pinho                  ****\n'),
	write('****                                               ****\n'),
	write('****                   3 - Voltar                  ****\n'),
    write('****                                               ****\n'),
    write('*******************************************************\n').

about_menu :- 
    clr,
    display_about_menu,
    get_code(Code),
    skip_line,    
    Choice is Code - 48,
    ( 
        Choice =:= 3 -> start_menu;
        
        about_menu  
    ).  

display_choose_game_menu :-
    display_header,
    write('****                                               ****\n'),
    write('****            1 - Jogador vs Jogador             ****\n'),
    write('****            2 - Jogador vs Computador          ****\n'),
    write('****            3 - Computador vs Computador       ****\n'),
    write('****                                               ****\n'),
    write('****                   4 - Voltar                  ****\n'),
    write('****                                               ****\n'),
    write('*******************************************************\n').

choose_game_menu :- 
    clr,
    display_choose_game_menu,
    get_number(Choice),
    ( 
        Choice =:= 1 -> start_game;
        Choice =:= 2 -> choose_level(human);
        Choice =:= 3 -> choose_level(computer);
        Choice =:= 4 -> start_menu;

        choose_game_menu        
    ).

display_choose_level_menu :-
    display_header,
    write('****                                               ****\n'),
    write('****                   1 - Facil                   ****\n'),
    write('****                   2 - Dificil                 ****\n'),
    write('****                                               ****\n'),
    write('****                   3 - Voltar                  ****\n'),
    write('****                                               ****\n'),
    write('*******************************************************\n').

choose_level(human) :- 
    clr,
    display_choose_level_menu,
    get_number(Choice),
    (
        Choice =:= 1 -> start_game(human1);
        Choice =:= 2 -> start_game(human2);
        Choice =:= 3 -> choose_game_menu;

        choose_level(human)
    ).

choose_level(computer) :- 
    clr,
    display_choose_level_menu,
    get_number(Choice),
    (
        Choice =:= 1 -> start_game(computer1);
        Choice =:= 2 -> start_game(computer2);
        Choice =:= 3 -> choose_game_menu;

        choose_level(computer) 
    ).

start_game(human1) :-
    asserta(is_game_over(false)),
    board(Board),
    choose_player(J),
    (
        J =:= 1 -> play(Board,1, human, computer1);
        J =:= 2 -> play(Board,1, computer1, human);
        J =:= 3 -> choose_game_menu
    ).

start_game(human2) :-
    asserta(is_game_over(false)),
    board(Board),
    choose_player(J),
    (
        J =:= 1 -> play(Board,1, human, computer2);
        J =:= 2 -> play(Board,1, computer2, human);
        J =:= 3 -> choose_game_menu
    ).

start_game(computer1) :-
    asserta(is_game_over(false)),
    board(Board),
    play(Board,1, computer1, computer1).

start_game(computer2) :-
    asserta(is_game_over(false)),
    board(Board),
    play(Board,1, computer2, computer2).

start_game :-
    asserta(is_game_over(false)),
    board(Board),
    choose_player(J),
    (
        J =:= 1 -> play(Board,1, human, human);
        J =:= 2 -> play(Board,1, human, human);
        J =:= 3 -> choose_game_menu
    ).

choose_player(Choice) :- 
    clr,
    display_header,
    write('\n O jogador com as pecas pretas e o primeiro a jogar.\n'),
    write(' Qual as pecas que prefere?\n'),
    write('   1 - Pretas  (Jogador 1)\n'),
    write('   2 - Brancas (Jogador 2)\n'),
    write('   3 - Voltar\n Escolha: '),
    get_number(Choice),
    check_choice_player(Choice).

check_choice_player(1).
check_choice_player(2).
check_choice_player(3).
check_choice_player(_) :-
    write('Erro: Escolha invalida.\n'),
    start_game.

game_over(Player, Pieces) :-
    retractall(catch(_)),

    new_line(1),
    write('*******************************************************\n'),
    write('**               VENCEDOR :: JOGADOR '), write(Player), write('               **\n'),
    write('*******************************************************\n'),
    write('  PECAS CAPTURADAS :\n'),
    display_catched_pieces(Player,Pieces),
    asserta(is_game_over(true)).

game_over(1) :-
    write('\nJOGADOR '),
    write(1),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    catched_pieces(2, Pieces),
    sort(Pieces, OrderedPieces),
    game_over(2, OrderedPieces).

game_over(2) :-
    write('\nJOGADOR '),
    write(2),
    write(' SEM MOVIMENTOS POSSIVEIS\n'),
    catched_pieces(1, Pieces),
    sort(Pieces, OrderedPieces),
    game_over(1, OrderedPieces).
