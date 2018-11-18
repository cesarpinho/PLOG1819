print_move(L-C-NewL-NewC-J, Lines) :-
    OldLin is Lines - L,
    OldCol is C + 65,
    NewLin is Lines - NewL,
    NewCol is NewC + 65,  
    write('Move : '),
    put_code(OldCol),
    write(OldLin),
    write(' -> '),
    put_code(NewCol),
    write(NewLin),
    new_line(2).

display_game(Board, Player, Lin, Col) :-
    new_line(1),
    display_col_num(Col),
    new_line(1),
    display_board(Board, Lin, Col),
    display_col_num(Col),
    new_line(2),
    write('--> Jogador '),
    write(Player),
    new_line(2).

display_col_num(N) :-
    space(6),
    Letter is 65,
    put_code(Letter),    
    space(2),
    N1 is N - 1,
    Next is Letter + 1,
    display_col_num(Next, N1).

display_col_num(_,0).
display_col_num(Letter, N) :-
    space(2),
    put_code(Letter),
    space(2),
    N1 is N - 1,
    Next is Letter + 1,
    display_col_num(Next, N1).

display_lin_num(N) :-
    write(N).


display_lin_separ(Col) :-
    space(3),
    write('------'),
    N1 is Col - 1,
    display_lin_separ(N1, Col).
display_lin_separ(N, Col) :-
    N > 0, !,
    write('-----'),
    N1 is N - 1,
    display_lin_separ(N1, Col).
display_lin_separ(0, _) :- nl.

display_col_separ :-
    write(' | ').

display_lin_num_and_col_separ(NumL) :-
    NumL > 0,
    space(1),
    display_lin_num(NumL),
    display_col_separ.

display_lin_num_and_col_separ(0) :-
    space(2),
    display_col_separ.

display_board([L|T], NumL, Col) :-
    display_lin_separ(Col),
    display_lin_num_and_col_separ(NumL),

    display_player_line(L),

    display_lin_num(NumL), nl,
    display_lin_num_and_col_separ(0),

    display_piece_line(L),

    new_line(1),
    NextL is NumL - 1,
    display_board(T, NextL, Col).

display_board([], _, Col) :- display_lin_separ(Col).

display_player_line([]).
display_player_line([C|L]) :-   
    display_player(C),
    display_col_separ,
    display_player_line(L).

display_piece_line([]).
display_piece_line([C|L]) :-
    display_piece(C),
    display_col_separ,
    display_piece_line(L).

display_player(0-0) :- space(2).
display_player(1-_) :- write('B'), space(1).
display_player(2-_) :- write('W'), space(1).

display_piece(0-0) :- space(2).
display_piece(_-X) :- X < 10, space(1), write(X).
display_piece(_-X) :- X >= 10, write(X).