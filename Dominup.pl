

startboard([ [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ] ]).
             
p1([0,11,21,30,32,40,42,44,51,53,55,61,63,65,70,72,74,76]). 
p2([10,20,22,31,33,41,43,50,52,54,60,62,64,66,71,73,75,77]).


/*prints*/
upperscore :- put_code(175).
space :- put_code(32).
barra :- put_code(124).

printlinenumber(0,_).
printlinenumber(X,Y) :- 
	write('  '),write(Y),write('   '),
	X1 is X - 1,
	Y1 is Y + 1,
	printlinenumber(X1,Y1).
	
printboardline(0).
printboardline(X) :-
	X > 0,
	write('-----|'),
	X1 is X - 1,
	printboardline(X1).

printblankcell :- write('     |').

printcell1([]).
printcell1([[],_,_]) :- 
	printblankcell.
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 1,
	barra,upperscore,upperscore,upperscore,upperscore,barra.
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 2,
	barra,upperscore,upperscore,upperscore,barra,barra.
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 3,
	upperscore,upperscore,upperscore,upperscore,barra,barra.
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 4,
	barra,space,space,space,barra,barra.

printcell2([]).
printcell2([[],_,_]) :- 
	printblankcell.
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 1,
	length([H1|T1],X),
	barra,write(V),put_char('-'),write(X),space,barra.
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 2,
	length([H1|T1],X),
	barra,write(V),put_char('-'),write(X),barra,barra.
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 3,
	length([H1|T1],X),
	space,write(V),put_char('-'),write(X),barra,barra.
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 4,
	length([H1|T1],X),
	barra,write(V),put_char('-'),write(X),barra,barra.
	
printcell3([]).
printcell3([[],_,_]) :- 
	printblankcell.
printcell3([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 1,
	write('|____|').
printcell3([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 2,
	write('|   ||').
printcell3([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 3,
	write('____||').
printcell3([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 4,
	write('|___||').

printline1([]).
printline1([ L1 | L2 ]) :- 
	printcell1(L1),
	printline1(L2).	
printline2([]).
printline2([ L1 | L2 ]) :- 
	printcell2(L1),
	printline2(L2).	
printline3([]).
printline3([ L1 | L2 ]) :- 
	printcell3(L1),
	printline3(L2).
			
printboard([],_).
printboard( [L1 | L2],Letter) :- 
	length(L1,X),
	space,space,barra,printline1(L1), nl,
	space,put_code(Letter),barra,printline2(L1), nl,
	space,space,barra,printline3(L1), nl,
	space,put_code(45),barra,printboardline(X),put_code(45),nl,
	Letter1 is Letter + 1,
	printboard(L2, Letter1).
	
printpiece1([]).
printpiece1(_Id) :- 
	barra,upperscore,upperscore,upperscore,upperscore,barra,
	upperscore,upperscore,upperscore,upperscore,barra.
printpiece2([]).
printpiece2(Id) :- 
	X is div(Id,10),
	Y is mod(Id,10),
	write('| '),write(X),write('  |  '),write(Y),write(' |').
printpiece3([]).
printpiece3(_Id) :- 
	write('|____|____|').

printlinepiece1([]).
printlinepiece1([H|T]) :-
	space, space,printpiece1(H), 
	printlinepiece1(T).

printlinepiece2([]).
printlinepiece2([H|T]) :-
	space,space,printpiece2(H),
	printlinepiece2(T).
	
printlinepiece3([]).
printlinepiece3([H|T]) :-
	space,space,printpiece3(H),
	printlinepiece3(T).
	
getnmembers([],_,_,_).
getnmembers(LI,X,LF1,LF2) :- 
	length(LF1, X),
	append(LF1, LF2, LI).
getnmembers(LI,_X,LI,[]).

printpiecenumber(Number):- 
	Number < 10,
	write('    ('),write(Number),write(')    ').
printpiecenumber(Number):- 
	Number >= 10,
	write('   ('),write(Number),write(')    ').
	
printpiecenumberline(_,[]).
printpiecenumberline(Number,[_H|T]):- 
	space,space,printpiecenumber(Number),
	NewNumber is Number + 1,
	printpiecenumberline(NewNumber,T).
	

printplayer([],_).	
printplayer(L,Number) :-
	getnmembers(L,6,LT1,LT2),
	printpiecenumberline(Number,LT1),nl,
	printlinepiece1(LT1),nl,
	printlinepiece2(LT1),nl,
	printlinepiece3(LT1),nl,nl,
	length(LT1,X),
	NewNumber is Number + X,
	printplayer(LT2,NewNumber).

printgame([]).
printgame([L1 | L2]-P1-_P2-CurrentPlayer) :- 
	nl,write('DOMINUP!'),nl,
	length(L1,X),
	nl,space,space,space,printlinenumber(X,1),nl,
	space,put_code(45),barra,printboardline(X),put_code(45), nl,
	printboard([L1 | L2],65),nl,nl,
	write('Player '),write(CurrentPlayer),write(' playing!'),nl,nl,
	printplayer(P1,1).
	
startgame :- 
        p1(L1), p2(L2), 
        startboard(B1),
        rotatePiece(10,P),
        putPiece(B1,1,1,P,1,Nb),
        length(Nb,X),
        addEmptyLine(Nb,X,B2),
        printgame(B2-L1-L2-1). 

midgame :- printgame([ 
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [12], 4 , [1] ],[ [3,12], 1 , [1,3] ], [ [3,2], 1 , [3,2] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [2], 0 , [4] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [1], 0 , [1] ], [ [1], 0 , [3] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ]
	]-
	[5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35]-
	1-1). 
/*
printgame([ [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ], [ [ [1], 1 , [2] ], [ [2], 4 , [1] ], [ [1], 5 , [3] ] ], [ [ [5], 1 , [4] ], [ [], -1 , [] ], [ [], -1 , [] ] ] ]).    
*/
generateEmptyLine(L,0,L).
generateEmptyLine(L,X,NewL) :- 
	append(L,[[ [], -1, [] ]],NewL1),
	X1 is X-1,
	generateEmptyLine(NewL1,X1,NewL).
	
generateEmptyLine(L,X):-
	generateEmptyLine([],X,L).

addEmptyLine([H|T],X,Newboard):-
	length(H,Size),
	generateEmptyLine(L,Size),
	length(H1,X),
	append(H1,H2,[H|T]),
	append(H1,[L|H2],Newboard).
	
addEmptyColumn([],_,[]).
addEmptyColumn([H|T],X,Newboard):- 
	length(L1,X),
	append(L1,L2,H),
	append( [ [] ,-1 ,[] ] ,L2,L3),
	addEmptyColumn(T,X,Newboard2),
	append(L1,L3,Newboard1),
	append(Newboard1,Newboard2,Newboard).
	

rotatePiece(Piece,NewPiece):- 
	V1 is div(Piece,10),
	V2 is mod(Piece,10),
	NewPiece is V2*10+V1.

putPiece(Board,Line,Column,Piece,Orientation,NewBoard) :- 
	Orientation =:= 0,
	Line1 is Line - 1,
	length(B1,Line1),
	append(B1,[L|B2],Board),
	Column1 is Column - 1,
	length(L1,Column1),
	append(L1,[[Id1,_,H1],[Id2,_,H2]|L2],L),
	V1 is div(Piece,10),
	V2 is mod(Piece,10),
	append([Piece],Id1,NewId1),
	append([Piece],Id2,NewId2),
	append([1],H1,NewH1),
	append([3],H2,NewH2),
	append(L1,[[NewId1,V1,NewH1],[NewId2,V2,NewH2]|L2],NewL),
	append(B1,[NewL|B2],NewBoard).
putPiece(Board,Line,Column,Piece,Orientation,NewBoard) :- 
	Orientation =:= 1,
	Line1 is Line - 1,
	length(B1,Line1),
	append(B1,[L,M|B2],Board),
	Column1 is Column - 1,
	length(L1,Column1),
	append(L1,[[Id1,_,H1]|L2],L),
	length(L3,Column1),
	append(L3,[[Id2,_,H2]|L4],M),
	V1 is div(Piece,10),
	V2 is mod(Piece,10),
	append([Piece],Id1,NewId1),
	append([Piece],Id2,NewId2),
	append([2],H1,NewH1),
	append([4],H2,NewH2),
	append(L1,[[NewId1,V1,NewH1]|L2],NewL),
	append(L3,[[NewId2,V2,NewH2]|L4],NewM),
	append(B1,[NewL,NewM|B2],NewBoard).


