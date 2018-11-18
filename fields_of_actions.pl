:- include('Interface.pl').
:- include('Logica.pl').
:- include('Menus.pl').
:- include('Utilitarios.pl').
:- include('Logica_computer.pl').

:- use_module(library(lists)).
:- use_module(library(between)).
:- dynamic is_game_over/1.
:- dynamic catch/1.


board([ [1-8 ,1-7, 1-6 ,1-5, 0-0, 0-0, 0-0, 0-0],
        [0-0, 0-0, 0-0, 0-0,1-12,1-11,1-10, 1-9],
        [1-4, 1-3, 1-2, 1-1, 0-0, 0-0, 0-0, 0-0],
        [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
        [0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0, 0-0],
        [0-0, 0-0, 0-0, 0-0, 2-1, 2-2, 2-3, 2-4],
        [2-9,2-10,2-11,2-12, 0-0, 0-0, 0-0, 0-0],
        [0-0, 0-0, 0-0, 0-0, 2-5, 2-6, 2-7, 2-8]]).

play :- 
    start_menu.
