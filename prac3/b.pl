%ProblemaB
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
