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

board( [
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [3], 4 , [1] ], [ [], -1 , [] ] ],
    [ [ [], -1 , [] ], [ [], -1 , [] ], [ [], -1 , [] ] ]
   ] ). /*empty board*/

joga(J,Tab) :- 
        %pecas(1,_), pecas(2,_),
        write('Jogador'), write(J),nl,
        board(Tab).

pecas(L) :- append([], [[1,0,0], [3,1,1]] ,L), 
        printL(L).

printL([H|_]) :- write(id(H)).

id(X) :- piece(X,_,_).

printPecas([]).
printPecas([H|T]) :-
        write(H),nl,
        printPecas(T).
        

inicio :- 
        intro,
        board(T),
        joga(1,T).


