piece(1,0,0).
piece(2,1,0).
piece(3,1,1).
piece(4,2,0).
piece(5,2,1).
piece(6,2,2).
piece(7,3,0).
piece(8,3,1).
piece(9,3,2).
piece(10,3,3).
piece(11,4,0).
piece(12,4,1).
piece(13,4,2).
piece(14,4,3).
piece(15,4,4).
piece(16,5,0).
piece(17,5,1).
piece(18,5,2).
piece(19,5,3).
piece(20,5,4).
piece(21,5,5).
piece(22,6,0).
piece(23,6,1).
piece(24,6,2).
piece(25,6,3).
piece(26,6,4).
piece(27,6,5).
piece(28,6,6).
piece(29,7,0).
piece(30,7,1).
piece(31,7,2).
piece(32,7,3).
piece(33,7,4).
piece(34,7,5).
piece(35,7,6).
piece(36,7,7).

/*cada celula contem list[id.piece] valor list[orientaçao] */
board( [
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [3], 4 , [1] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ]
   ] ). /*empty board*/
   
/*list de pieces*/
p1([]). 
p2([]).

/*contem o board,peças do p1 e p2, e o proximo jogador a jogar)*/


game(Board-P1-P2-Nextplayer).

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
printpiece1([_Id,_V1,_V2]) :- 
	barra,upperscore,upperscore,upperscore,upperscore,barra,
	upperscore,upperscore,upperscore,upperscore,barra.
printpiece2([]).
printpiece2([_Id,V1,V2]) :- 
	write('| '),write(V1),write('  |  '),write(V2),write(' |').
printpiece3([]).
printpiece3([_Id,_V1,_V2]) :- 
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


printplayer([]).	
printplayer(L) :-
	getnmembers(L,6,LT1,LT2),
	printlinepiece1(LT1),nl,
	printlinepiece2(LT1),nl,
	printlinepiece3(LT1),nl,nl,
	printplayer(LT2).

printgame([]).
printgame([L1 | L2]-P1-_P2-CurrentPlayer) :- 
	nl,write('DOMINUP!'),nl,
	length(L1,X),
	nl,space,space,space,printlinenumber(X,1),nl,
	space,put_code(45),barra,printboardline(X),put_code(45), nl,
	printboard([L1 | L2],65),nl,nl,
	write('Player '),write(CurrentPlayer),write(' playing!'),nl,nl,
	printplayer(P1).
	
startgame :- printgame([ 
	[ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ]
	]-
	[ [1,0,0],[3,1,1],[5,2,1],[7,3,0],[9,3,2],[11,4,0],[13,4,2],[15,4,4],[17,5,1],[19,5,3],[21,5,5],[23,6,1],[25,6,3],[27,6,5],[29,7,0],[31,7,2],[33,7,4],[35,7,6]]-
	1-1).    

midgame :- printgame([ 
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [12], 4 , [1] ],[ [3,12], 1 , [1,3] ], [ [3,2], 1 , [3,2] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [2], 0 , [4] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [1], 0 , [1] ], [ [1], 0 , [3] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ],
	[ [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ],[ [], -1 , [] ] ]
	]-
	[ [5,2,1],[7,3,0],[9,3,2],[11,4,0],[13,4,2],[15,4,4],[17,5,1],[19,5,3],[21,5,5],[23,6,1],[25,6,3],[27,6,5],[29,7,0],[31,7,2],[33,7,4],[35,7,6]]-
	1-1). 
/*
printgame([ [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ], [ [ [1], 1 , [2] ], [ [2], 4 , [1] ], [ [1], 5 , [3] ] ], [ [ [5], 1 , [4] ], [ [], -1 , [] ], [ [], -1 , [] ] ] ]).    
*/


