:- include('tabuleiro.pl').
:- include('Utilitarios.pl').


fields_of_action :-
    tabuleiro(Board),
    display_game(Board,1).