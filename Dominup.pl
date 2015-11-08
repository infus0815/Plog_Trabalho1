:-use_module(library(lists)).

%%%%% Inits %%%%%%

startboard([ [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ],
             [ [ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ], [ [], -1 , [] ],[ [], -1 , [] ] ] ]).
             
p1([0,11,21,30,32,40,42,44,51,53,55,61,63,65,70,72,74,76]). 
p2([10,20,22,31,33,41,43,50,52,54,60,62,64,66,71,73,75,77]).

%%%%% Prints %%%%%%%%

upperscore :- put_code(175).
space :- put_code(32).
barra :- put_code(124).

printlinenumber(0,_).
printlinenumber(X,Y) :- 
	Y < 10,
	write('  '),write(Y),write('   '),
	X1 is X - 1,
	Y1 is Y + 1,
	printlinenumber(X1,Y1).
printlinenumber(X,Y) :- 
	Y >= 10,
	write(' '),write(Y),write('   '),
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
printgame([L1 | L2]-P1-_P2-1) :-           
    length(L1,X),
    nl,space,space,space,printlinenumber(X,1),nl,
    space,put_code(45),barra,printboardline(X),put_code(45), nl,
    printboard([L1 | L2],97),nl,nl,
    write('Player '),write(1),write(' playing!'),nl,nl,
    printplayer(P1,1).
printgame([L1 | L2]-_P1-P2-2) :- 
    length(L1,X),
    nl,space,space,space,printlinenumber(X,1),nl,
	space,put_code(45),barra,printboardline(X),put_code(45), nl,
    printboard([L1 | L2],97),nl,nl,
    write('Player '),write(2),write(' playing!'),nl,nl,
    printplayer(P2,1).
		
printjogada(Line,Column,Piece,Orientation) :-
	nl,write('Jogada: '),nl,
	write('Peca - '),write(Piece),nl,
	Line1 is Line + 96,
	write('Linha - '),put_code(Line1),nl,
	write('Coluna - '),write(Column),nl,
	write('Orientacao - '),write(Orientation),nl,nl.


%%%%%%%% Game %%%%%%


startgame :-                        
        p1(L1), p2(L2),
        startboard(B1),
        nl,write('DOMINUP!'),nl,
        write('Humano:Humano (0), Humano:Computador (1), Computador:Computador (2)'),nl,
        read(Ch),
        startgame(B1,L1,L2,Ch).
startgame(B1,L1,L2,2) :-
        printgame(B1-L1-L2-2),
        nl,write('o Player 2 comeca a jogar com a peca 7-7 no centro da area de jogo:'),nl,nl,
        name('c3',[H|T]),
        Line is H-96,
        aToN(T,Column),
        verificaFim(0), nl,
        getPiecePlayer(2,L1,L2,18,Piece,X),
        verEmLista(2,L1,L2,Piece,P11,P22,X),
        putPiece(B1,Line,Column,Piece,0,Nb,X),
	checkBoardSize1(Nb,NNb),
        player(X,2,NPlayer,0),
        write('CPU Playing'),nl,nl,
        write('Introduz 1. para continuar'),nl,
        printjogada(Line,Column,Piece,0),
   	read(_),
        joga(NNb-NPlayer-P11-P22,0,3).
startgame(B1,L1,L2,1) :-
        printgame(B1-L1-L2-2),
        nl,write('o Player 2 comeca a jogar com a peca 7-7 no centro da area de jogo:'),nl,nl,
        name('c3',[H|T]),
        Line is H-96,
        aToN(T,Column),
        verificaFim(0), nl,
        getPiecePlayer(2,L1,L2,18,Piece,X),
        verEmLista(2,L1,L2,Piece,P11,P22,X),
        putPiece(B1,Line,Column,Piece,0,Nb,X),
	checkBoardSize1(Nb,NNb),
        player(X,2,NPlayer,0),
        write('CPU Playing'),nl,nl,
        printjogada(Line,Column,Piece,0),
        write('Introduz 1. para continuar'),nl,
   	read(_),
        joga(NNb-NPlayer-P11-P22,0,1).
startgame(B1,L1,L2,0) :-
        printgame(B1-L1-L2-2),
        nl,write('o Player 2 comeca a jogar com a peça 7-7 no centro da area de jogo:'),nl,nl,
        write('-> orientacao? (-1. -> sair)'),nl,
        name('c3',[H|T]),
        Line is H-96,
        aToN(T,Column),
        read(Or),  verificaFim(Or), nl,
        getPiecePlayer(2,L1,L2,18,Piece,X),
        verEmLista(2,L1,L2,Piece,P11,P22,X),
        putPiece(B1,Line,Column,Piece,Or,Nb,X),
	checkBoardSize1(Nb,NNb),
        player(X,2,NPlayer,0),
        printjogada(Line,Column,Piece,Or),
        joga(NNb-NPlayer-P11-P22,0,0).

joga(_Board-_CPlayer-_P1-_P2,-1,_).    % quit
joga(_Board-_CPlayer-_P1-_P2,1,_) :- write('O PLAYER 1 GANHOU O JOGO!!'),nl.     % ganha 1
joga(_Board-_CPlayer-_P1-_P2,2,_) :- write('O PLAYER 2 GANHOU O JOGO!!'),nl.     % ganha 2
joga(Board-CPlayer-P1-P2,0,0) :-          %HvsH  
	jogadahumana(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2,_),
	verificaGanha(NP1,NP2,Flag),
	joga(NBoard-NCPlayer-NP1-NP2,Flag,0).
joga(Board-CPlayer-P1-P2,0,1) :-          %HvsC  
	jogadahumana(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2,XY),
	verificaGanha(NP1,NP2,Flag),
	Flag2 is XY + 1,
	joga(NBoard-NCPlayer-NP1-NP2,Flag,Flag2).
joga(Board-CPlayer-P1-P2,0,2) :-          %HvsC  
	jogadacpu(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2),
	verificaGanha(NP1,NP2,Flag),
	joga(NBoard-NCPlayer-NP1-NP2,Flag,1).
joga(Board-CPlayer-P1-P2,0,3) :-          %CvsC  
	jogadacpu(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2),
	verificaGanha(NP1,NP2,Flag),
	joga(NBoard-NCPlayer-NP1-NP2,Flag,3).
	
    
jogadahumana(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2,XY) :-
	printgame(Board-P1-P2-CPlayer), nl,
	verifyNStackplay(Board,CPlayer,P1,P2,Count),
	write('Jogadas de stack possiveis - '),write(Count),nl,
	write('-> qual a peca que queres jogar? (-1. -> sair)'),nl,nl,
	read(NPiece), verificaFim(NPiece),nl,
	write('-> orientacao? (0 - hor, 1 - vert, 2 - hor switch, 3 - vert switch) (-1. -> sair)'),nl,
	read(Or),  verificaFim(Or), nl,
	write('-> onde? (-1. -> sair)'),nl,
	read(BuffPlace), verificaFim(BuffPlace), nl, name(BuffPlace,[H|T]),
	Line is H-96,
	aToN(T,Column),
	getPiecePlayer(CPlayer,P1,P2,NPiece,Piece,X),
	getOr(Piece,Or,NnPiece, NOr),
	verifyPlay(Board,Line,Column,NnPiece,NOr,Count,Y),
	XY is X/\Y,
	verEmLista(CPlayer,P1,P2,Piece,NP1,NP2,XY), 
	putPiece(Board,Line,Column,NnPiece,NOr,Nb,XY),
	checkBoardSize1(Nb,NBoard),
	player(XY,CPlayer,NCPlayer,Count),
	printjogada(Line,Column,NnPiece,NOr).
    
    
jogadacpu(Board-CPlayer-P1-P2,NBoard-NCPlayer-NP1-NP2) :- 
	printgame(Board-P1-P2-CPlayer), nl,
	verifyNStackplay(Board,CPlayer,P1,P2,Count),
   	write('Jogadas de stack possiveis - '),write(Count),nl,nl,
   	write('CPU Playing'),nl,nl,
   	getCurrentPlayer(CPlayer,P1,P2,CP),
   	generatePlay(Board,L,C,P,Or,CP,Count),
   	verEmLista(CPlayer,P1,P2,P,NP1,NP2,1),
   	putPiece(Board,L,C,P,Or,Nb,1),
   	checkBoardSize1(Nb,NBoard),
   	player(1,CPlayer,NCPlayer,Count),
   	printjogada(L,C,P,Or),
   	write('Introduz 1. para continuar'),nl,
   	read(_).
	

%%%%GETS%%%%

getCurrentPlayer(1,L,_,L).
getCurrentPlayer(2,_,L,L).

getCell(Board,Line,Column,Cell) :- 
	Line1 is Line - 1,
	Column1 is Column -1,
	length(L1,Line1),
	length(C1,Column1),
	append(L1,[H|_T],Board),
	append(C1,[Cell|_T2],H).

%getOr(Piece,Or,NPiece,NOr).
getOr(Piece,0,Piece,0).                         
getOr(Piece,1,Piece,1).
getOr(Piece,2,NPiece,0) :- rotatePiece(Piece, NPiece).
getOr(Piece,3,NPiece,1) :- rotatePiece(Piece, NPiece).

% getPiecePlayer(Pl,L1,L2,X,Piece)              
getPiecePlayer(1,L1,_L2,X,Piece,1) :-
    X > 0,
    X1 is X - 1,
	length(L1,Size),
	X1 < Size,
    length(Lx,X1),
    append(Lx,[Piece|_],L1).
getPiecePlayer(2,_L1,L2,X,Piece,1) :-
    X > 0, 
    X1 is X - 1,
	length(L2,Size),
	X1 < Size,
    length(Lx,X1),
    append(Lx,[Piece|_],L2).
getPiecePlayer(_,_L1,_L2,_X,_Piece,0) :- write('Peca Inexistente'),nl.

getPiece(Piece,P1,P2) :- 
	P1 is div(Piece,10),
	P2 is mod(Piece,10).
	
rotatePiece(Piece,NewPiece):- 
	V1 is div(Piece,10),
	V2 is mod(Piece,10),
	NewPiece is V2*10+V1.

not(X) :- X,!,fail.                             %%%%%%%% AULA PRATICA - NOT
not(_).

delete_one(X,L,L1) :-                           %%%%%%%% AULA PRATICA - DELETE ONE
    append(A,[X|B],L),
    append(A,B,L1).


%%%%%%%%%%Verifications + Changing Board functions %%%%%%%%%%
verificaGanha([],_L2,1).
verificaGanha(_L1,[],2).
verificaGanha(_,_,0).

verificaFim(-1) :- !,break.
verificaFim(_).



%verEmLista(CP,L1,L2,E,L,1,L11,L22)            
verEmLista(1,L1,L2,E,L,L2,1) :- delete_one(E,L1,L).   
verEmLista(2,L1,L2,E,L1,L,1) :- delete_one(E,L2,L).
verEmLista(1,L1,L2,E,L,L2,1) :- rotatePiece(E,NP),delete_one(NP,L1,L).   
verEmLista(2,L1,L2,E,L1,L,1) :- rotatePiece(E,NP),delete_one(NP,L2,L).
verEmLista(_,L1,L2,_,L1,L2,0).


player(0,1,1,_Count).                                  %%%%%%%%% nao muda
player(0,2,2,_Count). 
player(1,1,2,0).                                  %%%%%%%%% muda player 1 -> 2
player(1,2,1,0).                                  %%%%%%%%% muda player 2 -> 1
player(1,1,1,_Count).                                  %%%%%%%%% nao muda
player(1,2,2,_Count).                                   %%%%%%%%% nao muda

aToN([],Final,Final).                           %%%%%% ascii to number
aToN([H|T],Temp,Final):-
    T1 is H - 48,
    T2 is Temp * 10,
    T3 is T1 + T2,
    aToN(T,T3,Final).
        
aToN(T,Final):- 
	aToN(T,0,Final).


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
	append( [[ [] ,-1 ,[] ]] ,L2,L3),
	append(L1,L3,Newboard1),
	addEmptyColumn(T,X,Newboard2),
	append([Newboard1],Newboard2,Newboard).
	
checkEmptyLine([],0).
checkEmptyLine([H|T],Valor):- 
	checkEmptyLine(T,Valor),
	compareEmpty(H,[[],-1,[]]).
checkEmptyLine([_H|_T],1).

compareEmpty(H,H).

checkEmptyColumn([],_,0).
checkEmptyColumn([H|T],X,Valor):- 
	checkEmptyColumn(T,X,Valor),
	length(L1,X),
	append(L1,[M|_N],H),
	compareEmpty(M,[[],-1,[]]).
checkEmptyColumn([_H|_T],_X,1).


checkBoardSize1(Board,NewBoard):- 
	length(L1,1),
	append(L1,[H|_T],Board),
	checkEmptyLine(H,X),
	X =:= 0,
	checkBoardSize2(Board,NewBoard).
checkBoardSize1(Board,NewBoard):- 
	addEmptyLine(Board,0,NewBoard1),
	checkBoardSize1(NewBoard1,NewBoard).
	
checkBoardSize2(Board,NewBoard):- 
	length(Board,Bsize),
	Y is Bsize - 2,
	length(L1,Y),
	append(L1,[H|_T],Board),
	checkEmptyLine(H,X),
	X =:= 0,
	checkBoardSize3(Board,NewBoard).
checkBoardSize2(Board,NewBoard):- 
	length(Board,Bsize),
	addEmptyLine(Board,Bsize,NewBoard1),
	checkBoardSize2(NewBoard1,NewBoard).
	
checkBoardSize3(Board,NewBoard):- 
	checkEmptyColumn(Board,1,X),
	X =:= 0,
	checkBoardSize4(Board,NewBoard).
checkBoardSize3(Board,NewBoard):- 
	addEmptyColumn(Board,0,NewBoard1),
	checkBoardSize3(NewBoard1,NewBoard).
	
checkBoardSize4([H|T],[H|T]):- 
	length(H,NumCol),
	Y is NumCol - 2,
	checkEmptyColumn([H|T],Y,X),
	X =:= 0.
checkBoardSize4([H|T],NewBoard):-
	length(H,Y),
	addEmptyColumn([H|T],Y,NewBoard1),
	checkBoardSize4(NewBoard1,NewBoard).

	
verifyPlay([H|T],Line,Column,_Piece,Orientation,0,1) :- %%Type expand%%
	length([H|T],X),
	Line < X,
	Line > 0,
	length(H,Y),
	Column < Y,
	Y > 0,
	verifyExpandplay([H|T],Line,Column,Orientation).
verifyPlay([H|T],Line,Column,Piece,Orientation,Type,1) :-  %%Type stack%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	Type > 0,
	length([H|T],X),
	Line < X,
	Line > 0,
	length(H,Y),
	Column < Y,
	Y > 0,
	verifyStackplay([H|T],Line,Column,Piece,Orientation).
verifyPlay(_,_,_,_,_,_,0) :- write('Jogada Invalida!'),nl.




verifyHelper(Board,Piece,Line,Column,Orientation,1) :- 
	verifyStackplay(Board,Line,Column,Piece,Orientation).
verifyHelper(_,_,_,_,_,0).


verifyNStackplay(Board,_Piece,Line,_Column,Count,Count) :-
	length(Board,Y),
	Line > Y.
verifyNStackplay([H|T],Piece,Line,Column,TCount,Count) :-
	length(H,X),
	Column > X,
	NColumn is 1,
	NLine is Line + 1,
	verifyNStackplay([H|T],Piece,NLine,NColumn,TCount,Count).
verifyNStackplay(Board,Piece,Line,Column,TCount,Count) :- 
	verifyHelper(Board,Piece,Line,Column,0,A),
	verifyHelper(Board,Piece,Line,Column,1,B),
	rotatePiece(Piece,NPiece),
	verifyHelper(Board,NPiece,Line,Column,0,C),
	verifyHelper(Board,NPiece,Line,Column,1,D),
	R1 is A + B,
	R2 is C + D,
	R is R1 + R2,
	NTcount is TCount + R,
	NColumn is Column + 1,
	verifyNStackplay(Board,Piece,Line,NColumn,NTcount,Count).
	
%verifynStackplay(Board,CPlayer,P1,_P2,Count)
verifyNStackplay(_,1,[],_P2,0).
verifyNStackplay(Board,1,[H|T],_,Count) :- 
	verifyNStackplay(Board,H,1,1,0,TCount),
	verifyNStackplay(Board,1,T,_,NCount),
	Count is TCount + NCount.
verifyNStackplay(_,2,_P1,[],0).
verifyNStackplay(Board,2,_,[H|T],Count) :- 
	verifyNStackplay(Board,H,1,1,0,TCount),
	verifyNStackplay(Board,2,_,T,NCount),
	Count is TCount + NCount.
	
	
verifyStackplay(Board,Line,Column,Piece,Orientation) :- 
	Orientation =:= 0,
	Line1 is Line - 1,
	length(L1,Line1),
	append(L1,[H|_],Board),
	Column1 is Column - 1,
	length(H1,Column1),
	append(H1,[[T1,V1,_],[T2,V2,_] | _],H),
	getPiece(Piece,P1,P2),
	length(T1,A1),
	length(T2,A2),
	A1 =:= A2,
	P1 =:= V1,
	P2 =:= V2.
verifyStackplay(Board,Line,Column,Piece,Orientation) :- 
	Orientation =:= 1,
	Line1 is Line - 1,
	length(L1,Line1),
	append(L1,[H1|[H2|_]],Board),
	Column1 is Column - 1,
	length(H3,Column1),
	length(H4,Column1),
	append(H3,[[T1,V1,_]| _],H1),
	append(H4,[[T2,V2,_]| _],H2),
	getPiece(Piece,P1,P2),
	length(T1,A1),
	length(T2,A2),
	A1 =:= A2,
	P1 =:= V1,
	P2 =:= V2.



verifyExpand(Board,Line,Column,Orientation,1) :- 
	 Column > 0,
	 Line > 0,
	 getCell(Board,Line,Column,[_,_,[H|_T]]),
	 Z is mod(H,2),
	 Z =:= Orientation.
verifyExpand(_,_,_,_,0).
	
%verifyExpandplay(Board,Line,Column,Piece,Orientation)                
verifyExpandplay(Board,Line,Column,Orientation) :-
     	Orientation =:= 0,
	getCell(Board,Line,Column,Cell),
	Line1 is Line,
	Column1 is Column + 1,
	getCell(Board,Line1,Column1,Cell1),
	compareEmpty(Cell,[[],-1,[]]),
	compareEmpty(Cell1,[[],-1,[]]),
	C1 is Column - 1,
	L1 is Line,
	verifyExpand(Board,L1,C1,Orientation,R1),
	
	C2 is Column + 2,
	L2 is Line,
	verifyExpand(Board,L2,C2,Orientation,R2),
	
	C3 is Column,
	L3 is Line - 1,
	verifyExpand(Board,L3,C3,Orientation,R3),
	
	C4 is Column + 1,
	L4 is Line - 1,
	verifyExpand(Board,L4,C4,Orientation,R4),
	
	C5 is Column,
	L5 is Line + 1,
	verifyExpand(Board,L5,C5,Orientation,R5),
	
	C6 is Column + 1,
	L6 is Line + 1,
	verifyExpand(Board,L6,C6,Orientation,R6),
	
	X is R1\/R2\/R3\/R4\/R5\/R6,

        X =:= 1.
        
verifyExpandplay(Board,Line,Column,Orientation) :-
     	Orientation =:= 1,
	getCell(Board,Line,Column,Cell),
	Line1 is Line + 1,
	Column1 is Column,
	getCell(Board,Line1,Column1,Cell1),
	compareEmpty(Cell,[[],-1,[]]),
	compareEmpty(Cell1,[[],-1,[]]),
	C1 is Column,
	L1 is Line - 1,
	verifyExpand(Board,L1,C1,Orientation,R1),
	
	C2 is Column,
	L2 is Line + 2,
	verifyExpand(Board,L2,C2,Orientation,R2),
	
	C3 is Column - 1,
	L3 is Line,
	verifyExpand(Board,L3,C3,Orientation,R3),
	
	C4 is Column - 1,
	L4 is Line + 1,
	verifyExpand(Board,L4,C4,Orientation,R4),
	
	C5 is Column + 1,
	L5 is Line,
	verifyExpand(Board,L5,C5,Orientation,R5),
	
	C6 is Column + 1,
	L6 is Line + 1,
	verifyExpand(Board,L6,C6,Orientation,R6),
	
	X is R1\/R2\/R3\/R4\/R5\/R6,

        X =:= 1.




putPiece(Board,_Line,_Column,_Piece,_Orientation,Board,0). 
putPiece(Board,Line,Column,Piece,Orientation,NewBoard,1) :- 
	Orientation =:= 0,
	Line1 is Line - 1,
	length(B1,Line1),
	append(B1,[L|B2],Board),
	Column1 is Column - 1,
	length(L1,Column1),
	append(L1,[[Id1,_,H1],[Id2,_,H2]|L2],L),
	getPiece(Piece,V1,V2),
	append([Piece],Id1,NewId1),
	append([Piece],Id2,NewId2),
	append([1],H1,NewH1),
	append([3],H2,NewH2),
	append(L1,[[NewId1,V1,NewH1],[NewId2,V2,NewH2]|L2],NewL),
	append(B1,[NewL|B2],NewBoard).
putPiece(Board,Line,Column,Piece,Orientation,NewBoard,1) :- 
	Orientation =:= 1,
	Line1 is Line - 1,
	length(B1,Line1),
	append(B1,[L,M|B2],Board),
	Column1 is Column - 1,
	length(L1,Column1),
	append(L1,[[Id1,_,H1]|L2],L),
	length(L3,Column1),
	append(L3,[[Id2,_,H2]|L4],M),
	getPiece(Piece,V1,V2),
	append([Piece],Id1,NewId1),
	append([Piece],Id2,NewId2),
	append([2],H1,NewH1),
	append([4],H2,NewH2),
	append(L1,[[NewId1,V1,NewH1]|L2],NewL),
	append(L3,[[NewId2,V2,NewH2]|L4],NewM),
	append(B1,[NewL,NewM|B2],NewBoard).



%%%CPU%%%%
generatePlay(Board,Line,Column,Piece,Orientation,Plist,0) :- 
	generateExpandPlay(Board,Line,Column,Piece,Orientation,Plist).

generatePlay(Board,Line,Column,Piece,Orientation,Plist,_) :- 
	generateStackPlay(Board,Line,Column,Piece,Orientation,Plist).

	

generateStackPlay1(Board,Line,Column,Piece,Orientation,Line,Column,Piece,Orientation,_,1) :- 
	verifyStackplay(Board,Line,Column,Piece,Orientation).
generateStackPlay1(Board,TLine,_TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result) :-
	Count < 2,
	TOrientation =:= 1,
	length(Board,Size),
	TLine > Size,
	TTLine is 1,
	TTColumn is 1,
	TTOrientation is 0,
	rotatePiece(TPiece,TTPiece),
	NCount is Count + 1,
	generateStackPlay1(Board,TTLine,TTColumn,TTPiece,TTOrientation,Line,Column,Piece,Orientation,NCount,Result).
generateStackPlay1(Board,TLine,_TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result) :-
	Count < 2,
	TOrientation =:= 0,
	length(Board,Size),
	TLine > Size,
	TTLine is 1,
	TTColumn is 1,
	TTOrientation is TOrientation + 1,
	generateStackPlay1(Board,TTLine,TTColumn,TPiece,TTOrientation,Line,Column,Piece,Orientation,Count,Result).
generateStackPlay1([H|T],TLine,TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result) :-
	Count < 2,
	length(H,Size),
	TColumn > Size,
	TTLine is TLine + 1,
	TTColumn is 1,
	generateStackPlay1([H|T],TTLine,TTColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result).
generateStackPlay1(Board,TLine,TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result) :-
	Count < 2,
	TTColumn is TColumn + 1,
	generateStackPlay1(Board,TLine,TTColumn,TPiece,TOrientation,Line,Column,Piece,Orientation,Count,Result).
generateStackPlay1(_,_,_,_,_,_Line,_Column,_Piece,_Orientation,_,0).


generateStackPlay2(_Board,Line,Column,Piece,Orientation,Line,Column,Piece,Orientation,[_H|_T],1).
generateStackPlay2(Board,Line,Column,Piece,Orientation,_NLine,_NColumn,_NPiece,_NOrientation,[_H|T],0) :- 
	generateStackPlay(Board,Line,Column,Piece,Orientation,T).

generateStackPlay(Board,Line,Column,Piece,Orientation,[H|T]) :- 
	generateStackPlay1(Board,1,1,H,0,NLine,NColumn,NPiece,NOrientation,0,Result),
	generateStackPlay2(Board,Line,Column,Piece,Orientation,NLine,NColumn,NPiece,NOrientation,[H|T],Result).
	




generateExpandPlay(Board,Line,Column,Piece,Orientation,Line,Column,Piece,Orientation) :- 
	verifyExpandplay(Board,Line,Column,Orientation).	
generateExpandPlay(Board,TLine,_TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation) :-
	TOrientation =:= 0,
	length(Board,Size),
	TLine > Size,
	TTLine is 1,
	TTColumn is 1,
	TTOrientation is TOrientation + 1,
	generateExpandPlay(Board,TTLine,TTColumn,TPiece,TTOrientation,Line,Column,Piece,Orientation).
generateExpandPlay([H|T],TLine,TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation) :-
	length(H,Size),
	TColumn > Size,
	TTLine is TLine +1,
	TTColumn is 1,
	generateExpandPlay([H|T],TTLine,TTColumn,TPiece,TOrientation,Line,Column,Piece,Orientation).
generateExpandPlay(Board,TLine,TColumn,TPiece,TOrientation,Line,Column,Piece,Orientation) :-
	TTColumn is TColumn + 1,
	generateExpandPlay(Board,TLine,TTColumn,TPiece,TOrientation,Line,Column,Piece,Orientation).
generateExpandPlay(Board,Line,Column,Piece,Orientation,[H|_T]) :- 
	generateExpandPlay(Board,1,1,H,0,Line,Column,Piece,Orientation).

