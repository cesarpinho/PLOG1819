:- use_module(library(random)).

pieces([1,2,3,4,5,6,7,8,9,10,11,12]).

valid_moves(Board, Player, Lines, Columns, ListOfMoves) :- 
    valid_moves(Board, Player, 1, Lines, Columns, ListOfLists),
    append(ListOfLists, ListOfMoves).

valid_moves(_, _, 13, _, _, []).

valid_moves(Board, Player, Piece, Lines, Columns, [X|ListOfMoves]) :-
    exist_piece(Player, Board, Piece),
    possible_plays(Player, Board, Piece, Lines, Columns, X),
    NewPiece is Piece + 1,
    valid_moves(Board, Player, NewPiece, Lines, Columns, ListOfMoves).

valid_moves(Board, Player, Piece, Lines, Columns, ListOfMoves) :-
    Piece =< 12,
    NewPiece is Piece + 1,
    valid_moves(Board, Player, NewPiece, Lines, Columns, ListOfMoves).

choose_move(_, _, computer1, ListOfMoves, Move) :-
    random_member(Move, ListOfMoves).

choose_move(Board, Player, computer2, ListOfMoves, Move) :-
    board_size(Board, Lines, Columns),
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, _, Move).

choose_best_move(_, _, _, _, [], -1, _).
choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play =\= 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    value(Board, Player, FirstSeqPiece-LastSeqPiece, Value1),
    Value1 > 0,
    (
        Peca =:= FirstSeqPiece - 1;
        Peca =:= LastSeqPiece + 1
    ),
    Value2 is Value1 + 5,
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value3, Move2),
    save_move(Value2, Move1, Value3, Move2, Value, Move).

choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play =\= 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    value(Board, Player, FirstSeqPiece-LastSeqPiece, Value1),
    Value1 > 0,
    (
        Peca =:= FirstSeqPiece - 2;
        Peca =:= LastSeqPiece + 2
    ),
    Value2 is Value1 + 4,
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value3, Move2),
    save_move(Value2, Move1, Value3, Move2, Value, Move).

/* choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play =\= 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    value(Board, Player, FirstSeqPiece-LastSeqPiece, Value1),
    Value1 > 0,
    (
        Peca =:= FirstSeqPiece - 3;
        Peca =:= LastSeqPiece + 3
    ),
    Value2 is Value1 + 3,
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value3, Move2),
    save_move(Value2, Move1, Value3, Move2, Value, Move).

choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play =\= 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    value(Board, Player, FirstSeqPiece-LastSeqPiece, Value1),
    Value1 > 0,
    (
        Peca =:= FirstSeqPiece - 4;
        Peca =:= LastSeqPiece + 4
    ),
    Value2 is Value1 + 2,
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value3, Move2),
    save_move(Value2, Move1, Value3, Move2, Value, Move). */

choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play =\= 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    piece_position(Board, _-PecaToMove, L, C, Columns),
    value(Board, Player, _, Value1),
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value2, Move2),
    save_move(Value1, Move1, Value2, Move2, Value, Move).

choose_best_move(Board, Player, Lines, Columns, [L-C-NewL-NewC-Play|ListOfMoves], Value, Move) :-
    Play == 0,
    piece_position(Board, _-Peca, NewL, NewC, Columns),
    Value1 = 0,
    Move1 = L-C-NewL-NewC-Play,
    choose_best_move(Board, Player, Lines, Columns, ListOfMoves, Value2, Move2),
    save_move(Value1, Move1, Value2, Move2, Value, Move).


save_move(Value1, Move1, Value2, Move2, Value, Move) :-
    Value2 < Value1,
    Value = Value1,
    Move = Move1.

save_move(Value1, Move1, Value2, Move2, Value, Move) :-
    Value2 >= Value1,
    Value = Value2,
    Move = Move2.

move(Jog, Board, Lines, Columns, L-C-NewL-NewC-Play, NewBoard) :-
    piece_position(Board, _-Peca, L, C, Columns),
    replace(Board, Jog-Peca, NewL, NewC, Board1),
    empty_cel(V),
    replace(Board1, V, L, C, NewBoard),
    print_move(L-C-NewL-NewC-Play, Lines),
    is_catch(Jog, Board, Columns, L-C-NewL-NewC-Play).

value(Board, Player, FirstSeqPiece-LastSeqPiece, Value) :-
    pieces_out_of_game(Board, Player, Pieces),
    sort(Pieces, OrderedPieces),
    check_sequence(OrderedPieces, FirstSeqPiece-LastSeqPiece, Value).

pieces_out_of_game(Board, Player, CatchedPieces) :-
    append(Board, BoardList),
    pieces_playing(BoardList, Player, PiecesInGame),
    pieces(TotalPieces),
    subseq(TotalPieces, PiecesInGame, CatchedPieces).

pieces_playing([], _, []).
pieces_playing([Play-Piece|BoardList], 1, [P|PiecesInGame]) :-
    Play == 2,
    P = Piece,
    pieces_playing(BoardList, 1, PiecesInGame).

pieces_playing([Play-Piece|BoardList], 2, [P|PiecesInGame]) :-
    Play == 1,
    P = Piece,
    pieces_playing(BoardList, 2, PiecesInGame).

pieces_playing([Play-Piece|BoardList], Player, PiecesInGame) :-
    pieces_playing(BoardList, Player, PiecesInGame).
