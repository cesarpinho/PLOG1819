/* Imprime as jogadas possiveis */
display_plays(_, [], Indice, Indice).
display_plays(QuantLin, [_-_-NewL-NewC-Play|Plays], Indice, TotalJog) :-
    Lin is QuantLin - NewL,
    Col is NewC + 65,
    space(2),
    write(Indice),
    write(' -> '),
    put_code(Col),
    write(Lin),
    (Play =\= 0 -> write(' *Captura* ') ; true),
    new_line(1),
    NextI is Indice + 1,
    display_plays(QuantLin, Plays, NextI, TotalJog).

/* Imprime um movimento efetuado pelo computador */
print_move(L-C-NewL-NewC-_, Lines) :-
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

/* Imprme as pecas que foram capturads pelo vencedor do jogo */
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

/* Imprime o tabuleiro de jogo e a numeração de linhas e colunas */
display_game(Board, Player, Lin, Col) :-
    clr,
    display_header,
    new_line(1),
    display_col_num(Col),
    new_line(1),
    display_board(Board, Lin, Col),
    display_col_num(Col),
    new_line(2),
    write('--> Jogador '),
    write(Player),
    new_line(1),
    catched_pieces(Player, Pieces),
    sort(Pieces, OrderedPieces),
    write('  PECAS CAPTURADAS :\n'),
    display_catched_pieces(Player,OrderedPieces),
    new_line(1).

/* Imprime a numeração das colunas */
display_col_num(N) :-
    space(10),
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

/* Imprime a numeração das linhas */
display_lin_num(N) :-
    write(N).

/* Imprime o separador de linhas */
display_lin_separ(Col) :-
    space(7),
    write('------'),
    N1 is Col - 1,
    display_lin_separ(N1, Col).
display_lin_separ(N, Col) :-
    N > 0, !,
    write('-----'),
    N1 is N - 1,
    display_lin_separ(N1, Col).
display_lin_separ(0, _) :- nl.

/* Imprime o separador de colunas */
display_col_separ :-
    write(' | ').

/* Imprime o separador de colunas e o numero da linha */
display_lin_num_and_col_separ(NumL) :-
    NumL > 0,
    space(5),
    display_lin_num(NumL),
    display_col_separ.

display_lin_num_and_col_separ(0) :-
    space(6),
    display_col_separ.

/* Imprime o tabuleiro */
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

/* Imprime a Linha com os jogadores a que pertence cada peca */
display_player_line([]).
display_player_line([C|L]) :-   
    display_player(C),
    display_col_separ,
    display_player_line(L).

/* Imprime a linha com o numero da pecas */
display_piece_line([]).
display_piece_line([C|L]) :-
    display_piece(C),
    display_col_separ,
    display_piece_line(L).

/* Imprime um jogador */
display_player(0-0) :- space(2).
display_player(1-_) :- write('B'), space(1).
display_player(2-_) :- write('W'), space(1).

/* Imprime uma peca */
display_piece(0-0) :- space(2).
display_piece(_-X) :- X < 10, space(1), write(X).
display_piece(_-X) :- X >= 10, write(X).