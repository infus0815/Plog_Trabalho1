:-use_module(library(lists)).

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
printgame([L1 | L2]-P1-_P2-1) :-                %%%%%%%%%%%%%%%%%
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
        
/*startgame :- 
        p1(L1), p2(L2), 
        startboard(B1),
        rotatePiece(10,P),
        putPiece(B1,4,1,P,1,Nb),
                verifyStackplay(Nb,4,1,P,1,_F),
        %checkBoardSize1(Nb,B2),
        printgame(Nb-L1-L2-1). */
                
                
startgame :-                                    %%%%%%%%%%%%%%%%%
        p1(L1), p2(L2), 
        startboard(B1),
        nl,write('DOMINUP!'),nl,
        write('Humano:Humano (0), Humano:Computador (1), Computador:Computador (2)'),nl,
        read(Ch),
        startgame(B1,L1,L2,Ch).
startgame(B1,L1,L2,2) :-
        printgame(B1-L1-L2-2),
        nl,write('o Player 2 comeca a jogar com a peça 7-7 no centro da area de jogo:'),nl,nl,
        write('-> orientacao? (-1. -> sair)'),nl,
        name('c3',[H|T]),
        Line is H-96,
        aToN(T,Column),
        verificaFim(0), nl,
        getPiecePlayer(2,L1,L2,18,Piece),
        verEmLista(2,L1,L2,Piece,P11,P22,X),
        putPiece(B1,Line,Column,Piece,0,Nb,X),
        player(X,2,NPlayer),
        joga(Nb,NPlayer,P11,P22,0).
startgame(B1,L1,L2,_) :-
        printgame(B1-L1-L2-2),
        nl,write('o Player 2 comeca a jogar com a peça 7-7 no centro da area de jogo:'),nl,nl,
        write('-> orientacao? (-1. -> sair)'),nl,
        name('c3',[H|T]),
        Line is H-96,
        aToN(T,Column),
        read(Or),  verificaFim(Or), nl,
        getPiecePlayer(2,L1,L2,18,Piece,X),
        verEmLista(2,L1,L2,Piece,P11,P22),
        putPiece(B1,Line,Column,Piece,Or,Nb,X),
        player(X,2,NPlayer),
        joga(Nb,NPlayer,P11,P22,0).

joga(_Board, _CPlayer, _P1, _P2,-1).    % quit
joga(_Board, _CPlayer, _P1, _P2,1) :- write('O PLAYER 1 GANHOU O JOGO!!').     % ganha 1
joga(_Board, _CPlayer, _P1, _P2,2) :- write('O PLAYER 2 GANHOU O JOGO!!').     % ganha 2
joga(Board, CPlayer, P1, P2,0) :-                 %%%%%%%%%%%%%%%%%
        printgame(Board-P1-P2-CPlayer), nl,
    write('-> qual a peca que queres jogar? (-1. -> sair)'),nl,
    read(NPiece), verificaFim(NPiece),nl,
    write('-> orientacao? (0 - hor, 1 - vert, 2 - hor switch, 3 - vert switch) (-1. -> sair)'),nl,
    read(Or),  verificaFim(Or), nl,
    write('-> onde? (-1. -> sair)'),nl,
    read(BuffPlace), verificaFim(BuffPlace), nl, name(BuffPlace,[H|T]),
    Line is H-96,
    aToN(T,Column),
    getPiecePlayer(CPlayer,P1,P2,NPiece,Piece,X),
    verEmLista(CPlayer,P1,P2,Piece,P11,P22),
    getOr(Piece,Or,NnPiece, NOr),
        verifyPlay(Board,Line,Column,NnPiece,NOr,1,Y),
        XY is X/\Y,
    putPiece(Board,Line,Column,NnPiece,NOr,Nb,XY),
    player(XY,CPlayer,NPlayer),
    verificaGanha(P11,P22,Flag),
    joga(Nb, NPlayer, P11, P22,Flag).
        
        
verificaGanha([],_L2,1).
verificaGanha(_L1,[],2).
verificaGanha(_,_,0).

verificaFim(-1) :- !,break.
verificaFim(_).



%getOr(Piece,Or,NPiece,NOr).
getOr(Piece,0,Piece,0).                         %%%%%%%%%%%%%
getOr(Piece,1,Piece,1).
getOr(Piece,2,NPiece,0) :- rotatePiece(Piece, NPiece).
getOr(Piece,3,NPiece,1) :- rotatePiece(Piece, NPiece).

% getPiecePlayer(Pl,L1,L2,X,Piece)              %%%%%%%%%%%%%%%%%
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

%verEmLista(CP,L1,L2,E,L,1,L11,L22)             %%%%%%%%%%%%%%%%%
verEmLista(1,L1,L2,E,L,L2) :- !,delete_one(E,L1,L).   
verEmLista(2,L1,L2,E,L1,L) :- !,delete_one(E,L2,L).
verEmLista(1,L1,L2,E,L,L2) :- !,not(delete_one(E,L1,L)).
verEmLista(2,L1,L2,E,L1,L) :- !,not(delete_one(E,L2,L)).

player(0,1,1).                                  %%%%%%%%% nao muda
player(0,2,2).                                  %%%%%%%%% nao muda
player(1,1,2).                                  %%%%%%%%% muda player 1 -> 2
player(1,2,1).                                  %%%%%%%%% muda player 2 -> 1

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

        
verifyPlay([H|T],Line,Column,Piece,Orientation,0,1) :- %%Type expand%%
        length([H|T],X),
        Line < X,
        Line > 0,
        length(H,Y),
        Column < Y,
        Y > 0,
        verifyExpandplay([H|T],Line,Column,Piece,Orientation).
verifyPlay([H|T],Line,Column,Piece,Orientation,Type,1) :-  %%Type stack%%%
        Type > 0,
        length([H|T],X),
        Line < X,
        Line > 0,
        length(H,Y),
        Column < Y,
        Y > 0,
        verifyStackplay([H|T],Line,Column,Piece,Orientation).
verifyPlay(_,_,_,_,_,_,0) :- write('Jogada Invalida!'),nl.
        
        
verifyStackplay(Board,Line,Column,Piece,Orientation) :-  %%%%%%%%%FALTA VERIFICAR ALTURA%%%%%%%%%%%
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


verifyExpandplay(Board,Line,Column,Piece,Orientation) :- 
        append(L1,[H1|[H2|_]],Board).
        




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


