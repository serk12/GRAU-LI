%problemD
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