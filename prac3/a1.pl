%HacerAgus
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
