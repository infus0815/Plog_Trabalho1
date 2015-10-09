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
	put_code(124),put_code(175),put_code(175),put_code(175),put_code(175),put_code(124).
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 2,
	put_code(124),put_code(175),put_code(175),put_code(175),put_code(124),put_code(124).
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 3,
	put_code(175),put_code(175),put_code(175),put_code(175),put_code(124),put_code(124).
printcell1([[_H1|_T1],_V,[H2|_T2]]) :- 
	H2 =:= 4,
	put_code(124),put_code(32),put_code(32),put_code(32),put_code(124),put_code(124).

printcell2([]).
printcell2([[],_,_]) :- 
	printblankcell.
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 1,
	length([H1|T1],X),
	put_code(124),write(V),put_char('-'),write(X),put_code(32),put_code(124).
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 2,
	length([H1|T1],X),
	put_code(124),write(V),put_char('-'),write(X),put_code(124),put_code(124).
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 3,
	length([H1|T1],X),
	put_code(32),write(V),put_char('-'),write(X),put_code(124),put_code(124).
printcell2([[H1|T1],V,[H2|_T2]]) :- 
	H2 =:= 4,
	length([H1|T1],X),
	put_code(124),write(V),put_char('-'),write(X),put_code(124),put_code(124).
	
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
	put_code(32),put_code(32),put_code(124),printline1(L1), nl,
	put_code(32),put_code(Letter),put_code(124),printline2(L1), nl,
	put_code(32),put_code(32),put_code(124),printline3(L1), nl,
	put_code(32),put_code(45),put_code(124),printboardline(X),put_code(45),nl,
	Letter1 is Letter + 1,
	printboard(L2, Letter1).

printgame([]).
printgame( [L1 | L2]) :- 
	nl,write('DOMINUP!'),nl,
	length(L1,X),
	nl,put_code(32),put_code(45),put_code(124),printboardline(X),put_code(45), nl,
	printboard([L1 | L2],65),nl.

/*
[ [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ], [ [ [], -1 , [] ], [ [3], 4 , [1] ], [ [], -1 , [] ] ], [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ] ]
*/


