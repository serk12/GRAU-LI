
flatten([],[]).

flatten([[]|Y],L):- 
    flatten(Y,L),
    !.

flatten([X|Y],[X|L]):- 
    atomic(X), 
    flatten(Y,L),
    !.
    
flatten([X|Y],Z):- 
    flatten(X,A), 
    flatten(Y,L), 
    append(A,L,Z),
    !.

set([],[]).
set([H|T],[H|T1]):- 
    subtract(T, [H], T2), 
    set(T2,T1).

flattenNoRepetitions(X,L):- 
    flatten(X,A), 
    set(A,L).