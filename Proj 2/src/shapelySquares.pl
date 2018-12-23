%%%%% TYPE CODING %%%%%
% empty     :: 0 ::   %
% star		:: 1 :: S %
% square	:: 2 :: Q %
% diamond	:: 3 :: D %
% circle    :: 4 :: C %
% triangle	:: 5 :: T %
% knight	:: 6 :: K %
% heart		:: 7 :: H %
%%%%%%%%%%%%%%%%%%%%%%%

:- include('Utilitarios.pl').
:- include('puzzles.pl').
:- use_module(library(clpfd)).
:- use_module(library(lists)).

			
shapely_square(PuzzleNo) :-
	statistics(walltime,[Start|_]),
	
	abolish(circle_value/2),abolish(puzzle_size/2),
	puzzle(PuzzleNo,Puzzle,LineSums),
	length(Puzzle,YSize),
	nth1(1,Puzzle,Line),
	length(Line,XSize),
	asserta(puzzle_size(XSize,YSize)),
	new_puzzle(LineSums,SolvedPuzzle),

	asserta(circle_value(0,0)),
	apply_constraints(SolvedPuzzle,Puzzle,1,1),
	append(SolvedPuzzle,List),
	
	statistics(walltime,[BeforeLabeling|_]),
	labeling([],List),
	
	fd_statistics,
	statistics(walltime,[Stop|_]),
	
	ConstraintRuntime is BeforeLabeling - Start,
	LabelingRuntime is Stop - BeforeLabeling,  
	TotalRuntime is Stop - Start, 
	format('Runtime :\n  Before labeling : ~d\n  Labeling : ~d\n  Total : ~d\n',[ConstraintRuntime,LabelingRuntime,TotalRuntime]),
	display_game(Puzzle, SolvedPuzzle, LineSums).
	
	
new_puzzle([],[]).
new_puzzle([Sum1|Sums], Puzzle):-
	new_line(9,Sum1,Line),
	new_puzzle(Sums,P1),
	append([Line],P1,Puzzle).

new_line(Limit,Sum,Line):-
	puzzle_size(X,_),
	length(Line,X),
	domain(Line,0,Limit),
	sum(Line,#=,Sum).
	
%%%%% Constraint apply

apply_constraints(_,_,_,Y):-
	puzzle_size(_,YSize),
	Y #> YSize .

apply_constraints(SolvedPuzzle,Puzzle,X,Y):-
	puzzle_size(XSize,_),
	X #> XSize,
	NewY #= Y + 1,
	apply_constraints(SolvedPuzzle,Puzzle,1,NewY).

apply_constraints(SolvedPuzzle,Puzzle,X,Y):-
	nth1(Y,Puzzle,Line),
	nth1(X,Line,Type),
	apply_constraint_cell(Type,SolvedPuzzle,Puzzle,X,Y),
	NewX #= X + 1,
	apply_constraints(SolvedPuzzle,Puzzle,NewX,Y).

apply_constraint_cell(1,SolvedPuzzle,_,X,Y):-
	nth1(Y,SolvedPuzzle,Line),
	element(X, Line,V1), V1 in {2,3,5,7},
	PreviousY #= Y - 1,
	NextY #= Y + 1,
	PreviousX #= X - 1,
	NextX #= X + 1,
	star_constraint(SolvedPuzzle,X,PreviousY),
	star_constraint(SolvedPuzzle,PreviousX,Y),
	star_constraint(SolvedPuzzle,NextX,Y),
	star_constraint(SolvedPuzzle,X,NextY).

apply_constraint_cell(2,SolvedPuzzle,Puzzle,X,Y):-
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V), V in {0,5},
	PreviousY #= Y - 1,
	NextY #= Y + 1,
	PreviousX #= X - 1,
	NextX #= X + 1,
	square_constraint(SolvedPuzzle,Puzzle,X,PreviousY,V),
	square_constraint(SolvedPuzzle,Puzzle,PreviousX,Y,V),
	square_constraint(SolvedPuzzle,Puzzle,NextX,Y,V),
	square_constraint(SolvedPuzzle,Puzzle,X,NextY,V).

apply_constraint_cell(3,SolvedPuzzle,_,X,Y):-
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V),
	LeftSize #= X - 1,
	prefix_length(Line,LeftDigits,LeftSize),
	sum(LeftDigits,#=,V),
	V mod 2 #= 1.

apply_constraint_cell(4,SolvedPuzzle,_,X,Y):-
	circle_constraint(SolvedPuzzle,X,Y).

apply_constraint_cell(5,SolvedPuzzle,_,X,Y):-
	PreviousY #= Y - 1,
	!, % Tem que existir uma linha em cima de um triangulo.
	nth1(PreviousY,SolvedPuzzle,PreviousLine),
	element(X,PreviousLine,AboveV),
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V),
	AboveV #>= 2,
	AboveV mod 2 #= 0,
	V #> 0,
	V #< AboveV.

apply_constraint_cell(6,SolvedPuzzle,_,X,Y):-
	knight_constraint(SolvedPuzzle,X,Y,1,EvenCount),
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V),
	V #= EvenCount.

apply_constraint_cell(7,SolvedPuzzle,Puzzle,X,Y):-
	nth1(Y,SolvedPuzzle,Line),
	element(X, Line,V),
	PreviousY #= Y - 1,
	NextY #= Y + 1,
	PreviousX #= X - 1,
	NextX #= X + 1,
	heart_constraint(SolvedPuzzle,Puzzle,X,PreviousY,V1),
	heart_constraint(SolvedPuzzle,Puzzle,PreviousX,Y,V2),
	heart_constraint(SolvedPuzzle,Puzzle,NextX,Y,V3),
	heart_constraint(SolvedPuzzle,Puzzle,X,NextY,V4),
	V + V1 + V2 + V3 + V4 #= 10.

apply_constraint_cell(_,_,_,_,_).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Auxiliar Functions    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

check_puzzle_limits(X,Y):- 
	puzzle_size(XSize,YSize),
	X #=< XSize #/\ X #> 0 #/\ Y #=< YSize #/\ Y #> 0.

star_constraint(SolvedPuzzle,X,Y):-
	check_puzzle_limits(X,Y),
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V),
	V in {0,4,6,8,9}.
star_constraint(_,_,_).

square_constraint(SolvedPuzzle,Puzzle,X,Y,V):-
	check_puzzle_limits(X,Y),
	nth1(Y,Puzzle,L),
	nth1(X,L,Type),
	Type #\= 3,
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V1),
	V1 #\= V.
square_constraint(_,_,_,_,_).

circle_constraint(SolvedPuzzle,X,Y):-
	circle_value(X1,Y1),
	X1 > 0,
	nth1(Y1,SolvedPuzzle,Line1),
	element(X1,Line1,V),
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V1),
	V1 #= V.

circle_constraint(SolvedPuzzle,X,Y):-
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V),
	V mod 3 #\= 0,
	asserta((circle_value(X,Y))).

knight_attack_range(1,1,-2).
knight_attack_range(2,2,-1).
knight_attack_range(3,2,1).
knight_attack_range(4,1,2).
knight_attack_range(5,-1,2).
knight_attack_range(6,-2,1).
knight_attack_range(7,-2,-1).
knight_attack_range(8,-1,-2).

knight_constraint(_,_,_,9,0).
knight_constraint(SolvedPuzzle,X,Y,AttackId,Count):-
	knight_attack_range(AttackId,IncX,IncY),
	NewX #= X + IncX,
	NewY #= Y + IncY,
	nth1(NewY,SolvedPuzzle,Line),
	element(NewX,Line,V),
	(V mod 2 #= 0 )#<=>  C,
	NewAttackId #= AttackId + 1,
	knight_constraint(SolvedPuzzle,X,Y,NewAttackId,C1),
	Count #= C + C1.

knight_constraint(SolvedPuzzle,X,Y,AttackId,Count):-
	NewAttackId #= AttackId + 1,
	knight_constraint(SolvedPuzzle,X,Y,NewAttackId,Count).

heart_constraint(SolvedPuzzle,Puzzle,X,Y,V):-
	check_puzzle_limits(X,Y),
	nth1(Y,Puzzle,TypeLine),
	element(X, TypeLine,Type),
	Type #= 7,
	nth1(Y,SolvedPuzzle,Line),
	element(X,Line,V).
heart_constraint(_,_,_,_,0).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    Puzzle Generator    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate(XSize,YSize):-
	new_puzzle(XSize,YSize,Puzzle),
	length(LineSums,YSize),
	domain(LineSums,0,100),
	all_distinct(LineSums),
	asserta(puzzle_size(XSize,YSize)),
	new_puzzle(LineSums,SolvedPuzzle),

	asserta(circle_value(0,0)),
	apply_constraints(SolvedPuzzle,Puzzle,1,1),
	append(SolvedPuzzle,List),
	append(Puzzle,List1),
	global_cardinality(List1,Var),
	labeling([],List),
	display_game(Puzzle, SolvedPuzzle, LineSums).
	
new_puzzle(_,0,[]).
new_puzzle(XSize,YSize,Puzzle) :-
	new_line_types(7,XSize,Line),
	NextY is YSize - 1,
	new_puzzle(XSize,NextY,P1),
	append([Line],P1,Puzzle).

new_line_types(Limit,Length,Line):-
	length(Line,Length),
	domain(Line,0,Limit).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    BOARD DISPLAY    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
display_game(Puzzle, SolvedPuzzle, Sums) :-
	nth1(1,Puzzle,Line),
	length(Line,Col),
    display_puzzle(Puzzle,SolvedPuzzle,Sums,Col),nl.

/* Imprime o separador de linhas */
display_lin_separ(Col) :-
	space(1),
    write('-----'),
    N1 is Col - 1,
    display_lin_separ(N1, Col),
	write('       '),write('-----'),
	display_lin_separ(N1, Col), nl.
display_lin_separ(N, Col) :-
    N > 0, !,
    write('----'),
    N1 is N - 1,
    display_lin_separ(N1, Col).
display_lin_separ(0,_).

/* Imprime o tabuleiro */
display_puzzle([L|T],[Solved|SolvedPuzzle],[Sum|Sums] , Col) :-
    display_lin_separ(Col),
	write(' | '),
	display_line(puzzle,L),
	write_sum(Sum),
	write('    | '),
	display_line(solution,Solved),
	write_sum(Sum),
    new_line(1),
    display_puzzle(T, SolvedPuzzle, Sums, Col).

display_puzzle([], [], [], Col) :- display_lin_separ(Col).


/* Imprime a linha com o numero da pecas */
display_line(_,[]).
display_line(puzzle,[C|L]) :-
    write_type(C),
	write(' | '),
    display_line(puzzle,L).

display_line(solution,[C|L]) :-
    write(C),
	write(' | '),
    display_line(solution,L).

write_sum(Sum):- Sum >= 10, write(Sum).
write_sum(Sum):- Sum < 10, write(' '), write(Sum).

write_type(0):- write(' ').
write_type(1):- write('S').
write_type(2):- write('Q').
write_type(3):- write('D').
write_type(4):- write('C').
write_type(5):- write('T').
write_type(6):- write('K').
write_type(7):- write('H').
