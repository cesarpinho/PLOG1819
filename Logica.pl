/* Predicado principal de Jogo */
play(_, _, _, _) :-
    is_game_over(GameOver), GameOver == true,
    retractall(is_game_over(_)).

play(Board, Player, human, Type) :-
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Lines, Columns),
    display_game(Board, Player, Lines, Columns), repeat,
    choose_piece(Player, Board, Piece),
    possible_plays(Player, Board, Piece, Lines, Columns, Plays),
    check_quant_plays(Player, Board, Piece, Plays),
    choose_move(Player, Board, human, Type, Lines, Plays, Move),
    make_move(Player, Board, Piece, Columns, Move, NewBoard),
    change_player(Player, NewPlayer),
    play(NewBoard, NewPlayer, Type, human).

play(Board, Player, computer1, Type) :- 
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Lines, Columns),
    display_game(Board, Player, Lines, Columns),
    valid_moves(Board, Player, Lines, Columns, ListOfMoves),
    choose_move(Board, Player, computer1, ListOfMoves, Move),
    move(Player, Board, Lines, Columns, Move, NewBoard),
    wait_enter,
    change_player(Player, NewJ),
    play(NewBoard, NewJ, Type, computer1).

play(Board, Player, computer2, Type) :-
    is_game_over(GameOver), GameOver == false,
    board_size(Board, Lines, Columns),
    display_game(Board, Player, Lines, Columns),
    valid_moves(Board, Player, Lines, Columns, ListOfMoves),   
    choose_move(Board, Player, computer2, ListOfMoves, Move),
    move(Player, Board, Lines, Columns, Move, NewBoard),
    wait_enter,
    change_player(Player, NewJ),
    play(NewBoard, NewJ, Type, computer2).

/* Pede uma peca ao jogador, verifica se existe no tabuleiro e retorna-a*/
choose_piece(Player, Board, Piece) :-
    write('Escolha o numero da peca que pretende mover : '),
    get_number(Choice),
    new_line(1),
    exist_piece(Player,Board, Choice), !,
    Piece = Choice.
choose_piece(_, _, _) :-
    write('Erro: Peca nao existente.'),
    new_line(2),
    !,fail.

/* Verifica se a Piece existe no tabuleiro */
exist_piece(Player, Board, Piece) :-
    append(Board, List),
    member(Player-Piece, List).

/* Calcula as jogadas possiveis para a Piece */
possible_plays(Player, Board, Piece, Lines, Columns, Plays) :-
    piece_position(Board, Player-Piece, Lin, Col, Columns),
    adjacent_pieces(Board, Lin, Col, Lines, Columns, NumAdj),
    NumAdj =\= 0, !,
    move_horizontal(Player, Board, Lin, Col, Columns, NumAdj, MovH),
    move_vertical(Player, Board, Lin, Col, Lines, NumAdj, MovV),
    move_diagonal(Player, Board, Lin, Col, Lines, Columns, NumAdj, MovD),
    append(MovH, MovV, MovHV),
    append(MovHV, MovD, Plays).

possible_plays(Player, Board, Piece, Lines, Columns, ValidPlays) :-
    piece_position(Board, Player-Piece, Lin, Col, Columns),
    possible_plays_without_adjacents(Player, Board, Lin, Col, Lines, Columns, 1, Plays),
    replace(Board, 0-0, Lin, Col, Board1),
    filter_plays(Board1, Lines, Columns, Plays, ValidPlays).

/* Calcula as jogadas possiveis para uma peca da posicao (Lin,Col) quando esta nao tem pecas adjacentes */
possible_plays_without_adjacents(Player, Board, Lin, Col, Lines, Columns, NumAdj, Plays) :-
    NumAdj < Lines, 
    NumAdj < Columns,
    move_horizontal(Player, Board, Lin, Col, Columns, NumAdj, MovH),
    move_vertical(Player, Board, Lin, Col, Lines, NumAdj, MovV),
    move_diagonal(Player, Board, Lin, Col, Lines, Columns, NumAdj, MovD),
    NewNumAdj is NumAdj + 1,
    possible_plays_without_adjacents(Player, Board, Lin, Col, Lines, Columns, NewNumAdj, Plays2),
    append(MovH, MovV, MovHV),
    append(MovHV, MovD, Plays1),
    append(Plays1, Plays2, Plays).

possible_plays_without_adjacents(_, _, _, _, _, _, _, []). 

/* Retorna apenas as jogadas que resultam com pelo menos 2 pecas adjacentes.
Isto é aplicado quando uma peca nao tem pecas adjacentes */
filter_plays(Board, Lines, Columns, [L-C-NewL-NewC-Play|Plays], [X|List]) :- 
    Play =:= 0,
    adjacent_pieces(Board, NewL, NewC, Lines, Columns, NumAdj),
    NumAdj >= 2,
    X = L-C-NewL-NewC-Play,
    filter_plays(Board, Lines, Columns, Plays, List).

filter_plays(Board, Lines, Columns, [_|Plays], List) :- 
    filter_plays(Board, Lines, Columns, Plays, List).

filter_plays(_, _, _, [], []).

/* Verifica se a lista tem jogadas, caso contrario
verifica se outras pecas ainda têm movimentos possiveis */
check_quant_plays(Player, Board, []) :-
    Piece = 1,
    check_other_pieces(Player, Board, Piece).
check_quant_plays(_, _, _).

check_quant_plays(Player, Board, Piece, []) :-
    P is Piece + 1,
    check_other_pieces(Player, Board, P),
    write('A peca escolhida nao tem movimentos possiveis. \n\n'),
    !, fail.
check_quant_plays(_, _, _, L) :- tail(L, _).

/* Se nao exitir jogadas possiveis para nenhuma peca, é game over */
check_other_pieces(Player, _, 13) :- !, game_over(Player). 
check_other_pieces(Player, Board, Piece):-
    exist_piece(Player, Board, Piece),
    board_size(Board, Lines, Columns),
    possible_plays(Player, Board, Piece, Lines, Columns, Plays),
    check_quant_plays(Player, Board, Piece, Plays).

/* Apresenta os movimentos possiveis e pede ao jogador para escolher um */
choose_move(_, _, _, _, Lines, Plays, Move) :-
    write(' Jogadas possiveis :\n'),
    display_plays(Lines, Plays, 1, TotalJog),
    new_line(1),
    space(2),
    write('0 -> Voltar\n'),
    write('Escolha : '),
    peek_code(Code),
    Code =\= 48,
    get_number(Choice),
    Choice > 0,
    Choice < TotalJog, 
    nth1(Choice, Plays, Move), !.

choose_move(Player, Board, Type1, Type2, _, _, _) :-
    get_number(Choice), 
    Choice =:= 0, !,
    play(Board, Player, Type1, Type2).

choose_move(Player, Board, Type1, Type2, Lines, Plays, Move) :-
    write('Erro: Escolha invalida.\n\n'),
    choose_move(Player, Board, Type1, Type2, Lines, Plays, Move).

/* Move a peca no tabuleiro e verifica se foi capturada alguma peca */
make_move(Player, Board, Piece, Columns, L-C-NewL-NewC-Play, NewBoard) :- 
    replace(Board, Player-Piece, NewL, NewC, Board1),
    empty_cel(V),
    replace(Board1, V, L, C, NewBoard),
    is_catch(Player, Board, Columns, L-C-NewL-NewC-Play).

/* Verifica se foi capturada alguma peca, adiciona-a á base de dados e verifica se foi vitoria */
is_catch(_, _, _, _-_-_-_-0).
is_catch(Player, Board, Columns, _-_-NewL-NewC-Play) :-
    Play =\= 0, Play =\= Player,
    piece_position(Board, Play-Piece, NewL, NewC, Columns),    
    write(' * Peca '),
    write(Piece),
    write(' capturada *\n'),
    asserta(catch(Player-Piece)),
    check_vitory(Player).

/* Verifica se é vitoria do jogador */
check_vitory(Player) :-
    catched_pieces(Player, Pieces),
    sort(Pieces, OrderedPieces),
    check_sequence(OrderedPieces, _, Value),
    Value >= 5,
    game_over(Player, OrderedPieces).

check_vitory(_).

/* Verifica se existe alguma sequencia de numeros, e retorna a maior sequencia existente */
check_sequence([X|Pieces], FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    check_sequence([X|Pieces], 1, X-X, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq), !.

check_sequence([], _, _, 0-0, 0).
check_sequence([X|Pieces], Quant, FirstP-LastP, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    NextPiece is LastP + 1,
    NextPiece =:= X, 
    NextQ is Quant + 1,
    check_sequence(Pieces, NextQ, FirstP-X, FirstSeqPiece-LastSeqPiece, Seq),
    save_sequence(NextQ, FirstP-X, FirstSeqPiece-LastSeqPiece, Seq, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq).

check_sequence([X|Pieces], _, _-LastP, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    NextPiece is LastP + 1,
    NextPiece =\= X,
    NextQ is 1,
    check_sequence(Pieces, NextQ, X-X, FirstSeqPiece-LastSeqPiece, Seq),
    save_sequence(NextQ, X-X, FirstSeqPiece-LastSeqPiece, Seq, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq).

save_sequence(Quant, FirstP-LastP, _, SeqOrder, FirstGreatSeqPiece-LastGreatSeqPiece, GreaterSeq) :-
    Quant > SeqOrder,
    GreaterSeq = Quant,
    FirstGreatSeqPiece = FirstP,
    LastGreatSeqPiece = LastP.
save_sequence(_, _, OldFirstPiece-OldLastPiece, SeqOrder, OldFirstPiece-OldLastPiece, SeqOrder).

/* Retorna todas as pecas capturadas pelo Player */
catched_pieces(Player, CatchedPieces) :-
    catched_pieces([], CatchedPieces, Player).

catched_pieces(L1,L,Player) :- 
    catch(Player-X),
    not(member(X, L1)),
    append(L1,[X],List),
    catched_pieces(List,L,Player). 

catched_pieces(L,L,_).

%%%-----------------------------------------------------%%%
%%%   Calcula o numero de pecas adjacentes a uma peca   %%% 
%%%-----------------------------------------------------%%%
/* Retorna a posicao de uma peca.
Pode obter-se a peca numa posicao (L,C) ou a posicao (L,C) de uma peca */
piece_position(Board, Play-Piece, L, C, Col) :-
    append(Board, BoardList),
    nth0(Num, BoardList, Play-Piece),
    L is div(Num, Col),
    C is mod(Num, Col),!.

/* Calcula o numero de pecas adjacentes a uma peca na posicao (L,C) 
Nos comentarios de cada predicado, é mostrada a situação da peca qe é analisada.
P representa a posicao da peca, e os numero em redor sao as casas do tabuleiro em redor da peca */
%        C1  C2  C3  %
%  L1    1   2   3   %
%  L2    4   P   5   %
%  L3    6   7   8   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L > 0, C > 0, L < Lines-1, C < Columns-1, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    is_ocuppied_position(Board,L1,C3,Columns,N3),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N4),
    is_ocuppied_position(Board,L2,C3,Columns,N5),
    %%  L3  %%
    is_ocuppied_position(Board,L3,C1,Columns,N6),
    is_ocuppied_position(Board,L3,C2,Columns,N7),
    is_ocuppied_position(Board,L3,C3,Columns,N8),
    NumAdj is N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8.

%        C1  C2 %
%  L1    P   1  %
%  L2    2   3  %
adjacent_pieces(Board, L, C, _, Columns, NumAdj) :-
    L =:= 0, C =:= 0, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C2,Columns,N1),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N2),
    is_ocuppied_position(Board,L2,C2,Columns,N3),
    NumAdj is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   P   %
%  L2    2   3   %
adjacent_pieces(Board, L, C, _, Columns, NumAdj) :-
    L =:= 0 , C =:= Columns-1, !, 
    L1 is L,
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N2),
    is_ocuppied_position(Board,L2,C2,Columns,N3),
    NumAdj is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L =:= Lines-1 , C =:= 0, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C2,Columns,N3),
    NumAdj is N1 + N2 + N3.

%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L =:= Lines-1 , C =:= Columns-1, !, 
    L1 is L - 1,
    L2 is L,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N3),
    NumAdj is N1 + N2 + N3.

%        C1  C2  C3  %
%  L1    1   P   2   %
%  L2    3   4   5   %
adjacent_pieces(Board, L, C, _, Columns, NumAdj) :-
    L =:= 0, C > 0, C < Columns-1, !, 
    L1 is L, 
    L2 is L + 1,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C3,Columns,N2),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N3),
    is_ocuppied_position(Board,L2,C2,Columns,N4),
    is_ocuppied_position(Board,L2,C3,Columns,N5),
    NumAdj is N1 + N2 + N3 + N4 + N5.

%        C1  C2  C3  %
%  L1    1   2   3   %
%  L2    4   P   5   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L =:= Lines-1, C > 0, C < Columns-1, !, 
    L1 is L - 1, 
    L2 is L,
    C1 is C - 1,
    C2 is C,
    C3 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    is_ocuppied_position(Board,L1,C3,Columns,N3),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N4),
    is_ocuppied_position(Board,L2,C3,Columns,N5),
    NumAdj is N1 + N2 + N3 + N4 + N5.

%        C1  C2  %
%  L1    1   2   %
%  L2    P   3   %
%  L3    4   5   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L > 0, L < Lines-1, C =:= 0, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C,
    C2 is C + 1,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C2,Columns,N3),
    %%  L3  %%
    is_ocuppied_position(Board,L3,C1,Columns,N4),
    is_ocuppied_position(Board,L3,C2,Columns,N5),
    NumAdj is N1 + N2 + N3 + N4 + N5.


%        C1  C2  %
%  L1    1   2   %
%  L2    3   P   %
%  L3    4   5   %
adjacent_pieces(Board, L, C, Lines, Columns, NumAdj) :-
    L > 0, L < Lines-1, C =:= Columns-1, !, 
    L1 is L - 1, 
    L2 is L,
    L3 is L + 1,
    C1 is C - 1,
    C2 is C,
    %%  L1  %%
    is_ocuppied_position(Board,L1,C1,Columns,N1),
    is_ocuppied_position(Board,L1,C2,Columns,N2),
    %%  L2  %%
    is_ocuppied_position(Board,L2,C1,Columns,N3),
    %%  L3  %%
    is_ocuppied_position(Board,L3,C1,Columns,N4),
    is_ocuppied_position(Board,L3,C2,Columns,N5),
    NumAdj is N1 + N2 + N3 + N4 + N5.

/* Verifica se uma posicao (L,C) do tabuleiro está ocupada ou nao.
Em caso afirmativo , Occupied = 1, senao, Occupied = 0 */
is_ocuppied_position(Board, L, C, Columns, Occupied) :-
    append(Board, BoardList),
    Num is (L * Columns) + C,
    nth0(Num, BoardList, Piece),
    empty_cel(Piece), !,
    Occupied = 0.
is_ocuppied_position(_, _, _, _, 1).

%%%---------------------------------------------------%%%
%%%     Calcula os movimentos possiveis de uma peca   %%% 
%%%---------------------------------------------------%%%
/* Horizontal Movement */
move_horizontal(Player, Board, LinP, ColP, Columns, NumAdj, MovH) :-
    nth0(LinP, Board, Line),
    move_right(Player, LinP, ColP, Line, Columns, NumAdj, MovR),
    move_left(Player, LinP, ColP, Line, NumAdj, MovL),
    append(MovL,MovR, MovH).

move_right(Player, LinP, ColP, Line, Columns, NumAdj, MovR) :-
    NewC is ColP + NumAdj,
    NewC < Columns,
    nth0(NewC, Line, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovR = [LinP-ColP-LinP-NewC-NewPlayer].
move_right(_, _, _, _, _, _, []).

move_left(Player, LinP, ColP, Line, NumAdj, MovL) :-
    NewC is ColP - NumAdj,
    NewC >= 0,
    nth0(NewC, Line, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovL = [LinP-ColP-LinP-NewC-NewPlayer].
move_left(_, _, _, _, _, []).

/* Vertical Movement */
move_vertical(Player, Board, LinP, ColP, Lines, NumAdj, MovV) :-
    move_up(Player, Board, LinP, ColP, NumAdj, MovU),
    move_down(Player, Board, LinP, ColP, Lines, NumAdj, MovD),
    append(MovU, MovD, MovV).

move_up(Player, Board, LinP, ColP, NumAdj, MovU) :-
    NewL is LinP - NumAdj,
    NewL >= 0,
    nth0(NewL, Board, NewLine),
    nth0(ColP, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovU = [LinP-ColP-NewL-ColP-NewPlayer].
move_up(_, _, _, _, _, []).

move_down(Player, Board, LinP, ColP, Lines, NumAdj, MovD) :-
    NewL is LinP + NumAdj,
    NewL < Lines,
    nth0(NewL, Board, NewLine),
    nth0(ColP, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovD = [LinP-ColP-NewL-ColP-NewPlayer].
move_down(_, _, _, _, _, _, []).

/* Diagonal Movement */
move_diagonal(Player, Board, LinP, ColP, Lines, Columns, NumAdj, MovD) :- 
    move_up_left(Player, Board, LinP, ColP, NumAdj, MovUL),
    move_up_right(Player, Board, LinP, ColP, Columns, NumAdj, MovUR),
    move_down_left(Player, Board, LinP, ColP, Lines, NumAdj, MovDL),
    move_down_right(Player, Board, LinP, ColP, Lines, Columns, NumAdj, MovDR),
    append(MovUL, MovUR, Mov1),
    append(Mov1, MovDL, Mov2),
    append(Mov2, MovDR, MovD).

move_up_left(Player, Board, LinP, ColP, NumAdj, MovUL) :- 
    NewL is LinP - NumAdj,
    NewL >= 0,
    NewC is ColP - NumAdj,
    NewC >= 0,
    nth0(NewL, Board, NewLine),
    nth0(NewC, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovUL = [LinP-ColP-NewL-NewC-NewPlayer].
move_up_left(_, _, _, _, _, []).

move_up_right(Player, Board, LinP, ColP, Columns, NumAdj, MovUR) :- 
    NewL is LinP - NumAdj,
    NewL >= 0,
    NewC is ColP + NumAdj,
    NewC < Columns,
    nth0(NewL, Board, NewLine),
    nth0(NewC, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovUR = [LinP-ColP-NewL-NewC-NewPlayer].
move_up_right(_, _, _, _, _, _, []).

move_down_left(Player, Board, LinP, ColP, Lines, NumAdj, MovDL) :-
    NewL is LinP + NumAdj,
    NewL < Lines,
    NewC is ColP - NumAdj,
    NewC >= 0,
    nth0(NewL, Board, NewLine),
    nth0(NewC, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovDL = [LinP-ColP-NewL-NewC-NewPlayer].
move_down_left(_, _, _, _, _, _, []).

move_down_right(Player, Board, LinP, ColP, Lines, Columns, NumAdj, MovDR) :-
    NewL is LinP + NumAdj,
    NewL < Lines,
    NewC is ColP + NumAdj,
    NewC < Columns,
    nth0(NewL, Board, NewLine),
    nth0(NewC, NewLine, NewPlayer-NewPiece),
    (empty_cel(NewPlayer-NewPiece) ; NewPlayer =\= Player),
    MovDR = [LinP-ColP-NewL-NewC-NewPlayer].
move_down_right(_, _, _, _, _, _, _, []).