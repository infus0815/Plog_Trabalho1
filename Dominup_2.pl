:- use_module(library(lists)).

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



intro :- write('DOMINUP!'),nl.

p1([1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35]).
p2([2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36]).

board( [
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [3], 4 , [1] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ]
   ] ). /*empty board*/

inicio :-
        p1(L1),p2(L2),
        board(Tab),
        joga(0-L1-L2-Tab).

joga(X-L1-L2-T) :-
        printplayer(L1),nl, printplayer(L2),
        passa(X,Y),
        joga(Y-L3-L4-T).

verificaGanha(L) :- length(L,X), X =:= 0.

fim(L) :- verificaGanha(L), exit.
fim(_).

passa(1,2).
passa(2,1). 
        

        

inicio :- 
        intro,
        board(T),
        joga(1,T).


