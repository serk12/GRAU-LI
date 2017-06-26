%problemC
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