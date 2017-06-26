%Caballo
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