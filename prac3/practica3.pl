%HacerAgus %ToTest
get4in8([C5, C8], [0,  S]):- S is C5+C8, S =< 8.            %pour5to8Cabe
get4in8([C5, C8], [R,  8]):- S is C5+C8, S > 8, R is S-8.   %pour5to8Sobra
get4in8([C5, C8], [S,  0]):- S is C5+C8, S =< 5.            %pour8to5Cabe
get4in8([C5, C8], [5,  R]):- S is C5+C8, S > 5, R is S-5.   %pour5to8Sobra
get4in8([_ , C8], [5, C8]).                                 %fill5
get4in8([C5,  _], [C5, 8]).                                 %fill8
get4in8([_ , C8], [0, C8]).                                 %empty5
get4in8([C5,  _], [C5, 0]).                                 %empty8

nat(0).
nat(N):- nat(M), N is M + 1.

mover( E,E, C,C ).
mover( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal ):-
     get4in8( EstadoActual, EstSiguiente ),
     \+member(EstSiguiente,CaminoHastaAhora),
     mover( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal ).

solucionAguas(S):- nat(N), mover(S,[_,4],[S],C), length(C, N), write(C),!.












%Caballo %ToTest
camino( E, E, C, C, _).
camino( EstadoActual, EstadoFinal, CaminoHastaAhora, CaminoTotal, Nsize ):-
    unPaso( EstadoActual, EstSiguiente ),
    enElTablero(EstSiguiente, Nsize),
    \+member(EstSiguiente,CaminoHastaAhora),
    camino( EstSiguiente, EstadoFinal, [EstSiguiente|CaminoHastaAhora], CaminoTotal, Nsize ).

unPaso([X,Y], [X2,Y2]):- X2 is X + 2, Y2 is Y + 1.
unPaso([X,Y], [X2,Y2]):- X2 is X - 2, Y2 is Y - 1.
unPaso([X,Y], [X2,Y2]):- X2 is X + 2, Y2 is Y - 1.
unPaso([X,Y], [X2,Y2]):- X2 is X - 2, Y2 is Y + 1.
unPaso([X,Y], [X2,Y2]):- X2 is X + 1, Y2 is Y + 2.
unPaso([X,Y], [X2,Y2]):- X2 is X - 1, Y2 is Y - 2.
unPaso([X,Y], [X2,Y2]):- X2 is X + 1, Y2 is Y - 2.
unPaso([X,Y], [X2,Y2]):- X2 is X - 1, Y2 is Y + 2.

enElTablero([X,Y], N):- X < (N + 1), Y < (N + 1).

solucionCaballo(N, S, F, P):- camino(S,F,[S],C,N), D is P + 1, length(C,D), write(C),!.
















%ProblemaB %To Test
%Input: [[X,Y],[X,Y]...]
%Out: [[A,B,C],...] A jump over B, on B dies, and lands in C

jugada([_], C, C).
jugada(BolasLeft, MovesUntilNow, EndGame):-
    unSalto(BolasLeft, Bmoved, Bkilled,BNmoved),
    updateTable(BolasLeft, Bmoved, BNmoved, Bkilled, NextTable),
    append(MovesUntilNow,[[Bmoved, Bkilled,BNmoved]],C),
    jugada(NextTable, C, EndGame).

moreLess(X,Z):- Z is X + 1.
moreLess(X,Z):- Z is X - 1.

moreEq(X,X).
moreEq(X,Z):- Z is X + 1.

nextTo([Y,X],[Y,Z]):- moreLess(X,Z).
nextTo([Y,X],[W,Z]):- W is Y - 1, N is X - 1, moreEq(N,Z).
nextTo([Y,X],[W,Z]):- W is Y + 1, moreEq(X,Z).

oposed([Y,X],[W,Z],[A,B]):- 
    N is W - Y,
    M is Z - X,
    A is W + N,
    B is Z + M.
    
unSalto(Game,BA,BB,BNA):-
    member(BA,Game),
    member(BB,Game),
    nextTo(BB,BA),
    oposed(BA,BB,BNA),
    \+member(BNA,Game).
    
updateTable(Game, Bmoved,BNmoved, Bkilled, NextGame):-
    inTable(BNmoved),
    subtract(Game,[Bmoved,Bkilled],Next),
    append([BNmoved],Next,NextGame).
    

inTable([Y,X]):- Y >= 1, X >= 1, Y < 6, X =< Y.

solve(S,C):- jugada(S,[],C).
solve([],_).

swap([A,B],[B,A]).















%problemC %To Test
%K is a list of products [NAMES] and L is the minumum list that contains all nutrients


addN(L,R,[R|L]):- \+member(R,L),!.
addN(L,_,L).

getList([HN|N],K,L):-isIn(HN,K,R), getList(N,K,T), addN(T,R,L).
getList([],_,[]).

isIn(N, [K | _],K):- product(K, A), member(N, A),!.
isIn(N, [_ | L],R):- isIn(N,L,R).

isPossible([HL|L],K):- isIn(HL,K,_), isPossible(L,K).
isPossible([], _).

shopping(K,L):- 
    findall((X),numNutrients(X),N),
    isPossible(N,K),
    getList(N,K,L).

%testingC
numNutrients(2).
numNutrients(8).
numNutrients(4).
product(milk,[2,4,6]).
product(meat,[1,8]).












%problemD %To Test
%K have pairs of roats [citaA,citiB] we make a tree in M unifying all cities with the less Km

distance([[CitieA,CitieB]|M], Dist):- road(CitieA,CitieB,Km), distance(M,Aux), Dist is Aux + Km.
distance([], 0).

getPath(A,B):- road(A,B,_).
getPath(A,B):- road(B,A,_).

other(B,L):- member(B,L).
other(B,[]):- cities(C), member(B,C).

choseBranch(Cities,FromNow,[A,B]):-
    member(A,Cities),
    cities(C),
    subtract(C,Cities,L),
    other(B,L),
    getPath(A,B),
    connected([A,B],FromNow).
    
in(A,M):- member([_,A],M).
in(A,M):- member([A,_],M).

connected(_,[]).
connected([A,_],M):- in(A,M).
connected([_,A],M):- in(A,M).


makeTrees([],C,C).
makeTrees(CitiesL,FromNow, Return):-
    choseBranch(CitiesL,FromNow,Next),
    subtract(CitiesL,Next,L),
    append([Next],FromNow,W),
    makeTrees(L,W,Return).
    


mainroads(K,M):-
    cities(Cit),
    makeTrees(Cit,[],M),
    distance(M,Dist), Dist < K,!.

%testingD
cities([1,2,3,4]).
road(1,2,10).
road(1,4,20).
road(2,3,25).
road(3,4,12).

