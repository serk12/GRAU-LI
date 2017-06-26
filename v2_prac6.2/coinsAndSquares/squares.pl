:- use_module(library(clpfd)).

%ejemplo(_, Big, [S1...SN]): how to fit all squares of sizes S1...SN in a square of size Big?
ejemplo(0,  3,[2,1,1,1,1,1]).
ejemplo(1,  4,[2,2,2,1,1,1,1]).
ejemplo(2,  5,[3,2,2,2,1,1,1,1]).
ejemplo(3, 19,[10,9,7,6,4,4,3,3,3,3,3,2,2,2,1,1,1,1,1,1]).
ejemplo(4,112,[50,42,37,35,33,29,27,25,24,19,18,17,16,15,11,9,8,7,6,4,2]).
ejemplo(5,175,[81,64,56,55,51,43,39,38,35,33,31,30,29,20,18,16,14,9,8,5,4,3,2,1]).

insideBigSquare(_,[],[]).
insideBigSquare(Big,[S|Sides],[V|Vars]):-
    Aux is Big - (S - 1),
    V in 1 .. Aux,
    insideBigSquare(Big,Sides,Vars).

main:- 
    ejemplo(3,Big,Sides),
    nl, write('Fitting all squares of size '), write(Sides), write(' into big square of size '), write(Big), nl,nl,
    length(Sides,N),
    length(ColVars,N),
    length(RowVars,N),
    insideBigSquare(Big,Sides,RowVars),
    insideBigSquare(Big,Sides,ColVars),
    nonoverlapping(Big,Sides,RowVars,ColVars),
    label(RowVars),label(ColVars),
    displaySol(Big,Sides,RowVars,ColVars), halt.


nonoverlapping(_,[],[],[]).
nonoverlapping(N,[S|Sides],[R|RowVars],[C|ColVars]):-
  check(S,R,C,Sides,RowVars,ColVars),
  nonoverlapping(N,Sides,RowVars,ColVars).

check(_,_,_,[],[],[]).
check(S1, R1, C1,[S2|Sides],[R2|RowVars],[C2|ColVars]):-
  R1+S1 #=< R2 #\/ R2+S2 #=< R1 #\/ C1+S1 #=< C2 #\/ C2+S2 #=< C1,
  check(S1,R1,C1,Sides,RowVars,ColVars).
    
displaySol(N,Sides,RowVars,ColVars):- 
    write(RowVars), write(ColVars),
    between(1,N,Row), nl, between(1,N,Col),
    nth1(K,Sides,S),    
    nth1(K,RowVars,RV),    RVS is RV+S-1,     between(RV,RVS,Row),
    nth1(K,ColVars,CV),    CVS is CV+S-1,     between(CV,CVS,Col),
    writeSide(S), fail.
displaySol(_,_,_,_):- nl,nl,!.

writeSide(S):- S<10, write('  '),write(S),!.
writeSide(S):-       write(' ' ),write(S),!.

