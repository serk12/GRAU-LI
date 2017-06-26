
              % NUM, COLOR, JOB, ANIMAL, BEBIDA, PAIS
casas:- Sol = [ [1,_,_,_,_,_],
                [2,_,_,_,_,_],
                [3,_,_,_,_,_],
                [4,_,_,_,_,_],
                [5,_,_,_,_,_]],
                member(X0, Sol), casaRojaPeru(X0),
                member(X1, Sol), francesPerro(X1),
                member(X2, Sol), chinoRon(X2),
                member(X3, Sol), hungaroPrimera(X3),
                member(X4, Sol), casaVerdeConac(X4),
                member(X5, Sol), escultorCaracoles(X5),
                member(X6, Sol), casaAmarillaActor(X6),
                member(X7, Sol), num3Cava(X7),
                member(X8, Sol), notarioWhisky(X8),
                member(X9, Sol), pintorJapones(X9),
                member(X17, Sol), member(X10, Sol), medicoLadoArdilla(X17,X10),
                member(X11, Sol), member(X12, Sol), hungaroLadoCasaAzul(X11,X12),
                member(X13, Sol), member(X14, Sol), casaVerdeIzquierdaBlanca(X13,X14),
                member(X15, Sol), member(X16, Sol), actorCaballo(X15,X16),
                %member(X18, Sol), algienLeche(X18),
                %member(X19, Sol), algienGato(X19),
                write(Sol), nl,!.
        
pintorJapones([_,_,pintor,_,_,japones]).
                
casaRojaPeru([_,rojo,_,_,_,peru]).

francesPerro([_,_,_,perro,_,frances]).

chinoRon([_,_,_,_,ron,chino]).

hungaroPrimera([1,_,_,_,_,hungaro]).

casaVerdeConac([_,verde,_,_,coñac,_]).

escultorCaracoles([_,_,escultor,caracoles,_,_]).

casaAmarillaActor([_,amarilla,actor,_,_,_]).
    
num3Cava([3,_,_,_,cava,_]).
    
notarioWhisky([_,_,notario,_,whisky,_]).
    
%algienLeche([_,_,_,_,leche,_]). %%CONSULTAR

%algienGato([_,_,_,gato,_,_]). %%CONSULTAR
    
next(N1,N2):- N1 is N2 -1,!.
next(N1,N2):- N1 is N2 +1,!.

actorCaballo([N1,_,actor,_,_,_],[N2,_,_,caballo,_,_]):- next(N1,N2).

casaVerdeIzquierdaBlanca([N1,blanca,_,_,_,_],[N2,verde,_,_,_,_]):- N1 is N2 - 1.
    
hungaroLadoCasaAzul([N1,azul,_,_,_,_],[N2,_,_,_,_,hungaro]):- next(N1,N2).
    
medicoLadoArdilla([N1,_,medico,_,_,_],[N2,_,_,ardilla,_,_]):- next(N1,N2).