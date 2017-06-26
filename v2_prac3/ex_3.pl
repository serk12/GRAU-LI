% 3. Escribe en Prolog un sencillo analizador sintactico para el lenguaje sumbol
append(A,B,C,Z):- append(A,X,Z),!, append(B,C,X).

programa(Z):-append([begin],X,[end],Z), instrucciones(X), !.

instrucciones(Z):- instruccion(Z),!.
instrucciones(Z):- append(A,[;],C,Z), instruccion(A), instrucciones(C),!.

instruccion(Z):-
    variable(A), 
    variable(C),
    append(A,[=],C,X), 
    %debug(Z),
    append(X,[+],E,Z),
    variable(E),!.
    
instruccion(Z):-
    append([if],B,[=],X), 
    append(X,D,[then],Y), 
    append(Y,F,[else],X1),
    append(X1, G, [endif], Z),
    variable(B), 
    variable(D), 
    instrucciones(F), 
    instrucciones(G),
    !.

variable([x]).
variable([y]).
variable([z]).