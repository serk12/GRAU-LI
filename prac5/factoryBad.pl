:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.

%% We have a factory of concrete products (beams, walls, roofs) that works permanently (168h/week).  
%% Every week we plan our production tasks for the following week.  
%% For example, one task may be to produce a concrete 
%% beam of a certain type, which takes 10 hours and requires (always one single unit of) the following resources: platform, crane, truck, mechanic, driver. 
%% But there are only a limited amount of units of each resource available. For example, we may have only 3 trucks.
%% We have 168 hours (numbered from 1 to 168) for all tasks, but we want to finish all tasks as soon as possible.

%%%%%%%%%%%%%%%%%%%%%%% Input. %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%% format:
%% task( taskID, Duration, ListOFResourcesUsed ).
%% resource( resourceID, NumUnitsAvailable ).
:-include(hardmayBe147).  % simple input example file. Try the two given harder ones too!

%%%%%% Some helpful definitions to make the code cleaner:

task(T):-                       task(T,_,_).
duration(T,D):-                 task(T,D,_).
resource(R):-                   resourceUnits(R,_).
time(Time, H):-                 between(1, Time, H).
usesResource(T,R):-             task(T,_,L), member(R,L).
numResource(T,U):-              task(T,_,Rs), length(Rs, U).
accesibleTime(MaxTime,T,H):-    duration(T,D), Time is MaxTime - D + 1, time(Time, H).

% We use the following types of symbolic propositional variables:
%  1. start-T-H   means: "task T starts at hour H"     (MANDATORY)
%  2. inUse-R-T-H means: "one resource R is in use at hour H for task T"

writeClauses(MaxTime):- 
    initClauseGeneration,
    eachTaskStartsOnce(MaxTime),
    usingRecourceInHour(MaxTime),
    allTaskOnOneRow(MaxTime),
    linkAllResourchesHour(MaxTime),
    true,!.
    
eachTaskStartsOnce(MaxTime):-
    task(T), 
    findall(start-T-H, accesibleTime(MaxTime,T,H), Lits),
    exactly(1, Lits),
    %debug(Lits),
    fail.
eachTaskStartsOnce(_).

usingRecourceInHour(MaxTime):- %for all pair H-R at most #R 
    time(MaxTime, H), %Ineficiency?
    resource(R),
    findall(inUse-R-T-H, usesResource(T,R), Lits),
    resourceUnits(R,D),
    %debug(D),
    atMost(D,Lits),
    %debug(Lits),
    fail.
usingRecourceInHour(_).


%if (inUse-_-T-H) then all [inUse-R-T-H] where R is in the task T be true
linkAllResourchesHour(MaxTime):- 
    task(T),
    usesResource(T,R1),
    usesResource(T,R2),
    R1 > R2,
    time(MaxTime, H), %Ineficiency?
    writeClause([  inUse-R1-T-H, \+inUse-R2-T-H]), 
    writeClause([\+inUse-R1-T-H,   inUse-R2-T-H]),
    fail.
linkAllResourchesHour(_).

%%if (start-T-H) then only be true all in [inUse-_-T-H,...,inUse-_-T-(H+N)] where N is hours of work
allTaskOnOneRow(MaxTime):- 
    task(T),
    accesibleTime(MaxTime,T,H),
    midleHours(T,M),
    H2 is H + M,
    H2 >= H,
    usesResource(T,R),
    writeClause([\+start-T-H,   inUse-R-T-H2]),
    %writeClause([  start-T-H, \+inUse-R-T-H2]), ?why not?
    %debug([start-T-H, inUse-R-T-H2]),  
    fail.
allTaskOnOneRow(_).
    
midleHours(T,M):-
    duration(T,D),
    Aux is D - 1,
    between(0 ,Aux,M).

initClauseGeneration:-  %initialize all info about variables and clauses:
    retractall(numClauses(   _)), 
    retractall(numVars(      _)), 
    retractall(varNumber(_,_,_)),
    assert(numClauses( 0 )), 
    assert(numVars(    0 )),     !.

%%%%%%%%%%%%%%%%%%%%% Auxiliary predicates for displaying the solutions and for counting the hours used:

hoursUsed(M,K):- findall(End, ( member(start-T-H,M), duration(T,D), End is H+D-1), L),  max_list(L,K),!.

displaySol(M):- write(M),nl,fail. %%%FOR DEBUG USE

displaySol(_):- nl,nl,   write('    '),
    write('        10        20        30        40        50        60        70        80'),
    write('        90       100       110       120       130       140       150       160'),nl, write('    '),
    write('12345678901234567890123456789012345678901234567890123456789012345678901234567890'),
    write('1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678'),nl,fail.
displaySol(M):- task(T), writeNum2(T), member(start-T-H,M), duration(T,D),
		B is H-1, writeX(' ',B), writeX('x',D), nl, fail.
displaySol(_):- nl,nl,!.

writeX(_,0):-!.
writeX(X,N):- write(X), N1 is N-1, writeX(X,N1),!.

writeNum2(T):-T<10, write(' '), write(T), write(': '), !.
writeNum2(T):-                  write(T), write(': '), !.

showSolIfBelow(Low,K,M):- K<Low, displaySol(M),!.
showSolIfBelow(_,_,_).

%%%%%%%%%%%%%%%%%%%%%  main:

main:-  symbolicOutput(1), !, writeClauses(30), halt.   % print the clauses in symbolic form and halt
main:-  
    tell(clauses), writeClauses(168), told,     % generate the (numeric) SAT clauses and call the solver
    tell(header),  writeHeader,  told,
    numVars(N), numClauses(C), 
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Looking for initial plan that may take the whole week (168h).'), nl,
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,[]),!.

treatResult(20,[]       ):- write('No solution exists.'), nl, halt.
treatResult(20,BestModel):- nl,nl,write('Optimal solution: '),nl, displaySol(BestModel), halt.
treatResult(10,_):-
    see(model), symbolicModel(M), seen,  
    hoursUsed(M,K),
    write('plan found that takes '), write(K), write(' hours '),nl,nl, K1 is K-1,
    showSolIfBelow(200,K,M),
    tell(clauses), writeClauses(K1), told,
    tell(header),  writeHeader,  told,
    numVars(N),numClauses(C),
    write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
    shell('cat header clauses > infile.cnf',_),
    write('Launching picosat...'), nl,
    shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
    treatResult(Result,M),!.


writeClause([]):- symbolicOutput(1),!, nl.
writeClause([]):- countClause, write(0), nl.
writeClause([Lit|C]):- w(Lit), writeClause(C),!.
w( Lit ):- symbolicOutput(1), write(Lit), write(' '),!.
w(\+Var):- var2num(Var,N), write(-), write(N), write(' '),!.
w(  Var):- var2num(Var,N),           write(N), write(' '),!.


% given the symbolic variable V, find its variable number N in the SAT solver:
var2num(V,N):- hash_term(V,Key), existsOrCreate(V,Key,N),!.
existsOrCreate(V,Key,N):- varNumber(Key,V,N),!.                            % V already existed with num N
existsOrCreate(V,Key,N):- newVarNumber(N), assert(varNumber(Key,V,N)), !.  % otherwise, introduce new N for V

writeHeader:- numVars(N),numClauses(C), write('p cnf '),write(N), write(' '),write(C),nl.

countClause:-     retract( numClauses(N0) ), N is N0+1, assert( numClauses(N) ),!.
newVarNumber(N):- retract( numVars(   N0) ), N is N0+1, assert(    numVars(N) ),!.
 
% Getting the symbolic model M from the output file:
symbolicModel(M):- get_code(Char), readWord(Char,W), symbolicModel(M1), addIfPositiveInt(W,M1,M),!.
symbolicModel([]).
addIfPositiveInt(W,L,[Var|L]):- W = [C|_], between(48,57,C), number_codes(N,W), N>0, varNumber(_,Var,N),!.
addIfPositiveInt(_,L,L).
readWord( 99,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ c
readWord(115,W):- repeat, get_code(Ch), member(Ch,[-1,10]), !, get_code(Ch1), readWord(Ch1,W),!. % skip line starting w/ s
readWord(-1,_):-!, fail. %end of file
readWord(C,[]):- member(C,[10,32]), !. % newline or white space marks end of word
readWord(Char,[Char|W]):- get_code(Char1), readWord(Char1,W), !.
%========================================================================================

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Everything below is given as a standard library, reusable for solving 
%    with SAT many different problems.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%% Cardinality constraints on arbitrary sets of literals Lits:
% For example the following generates the clauses expressing that 
%     exactly K literals of the list Lits are true:
exactly(K,Lits):- atLeast(K,Lits), atMost(K,Lits),!.

atMost(K,Lits):-   % l1+...+ln <= k:  in all subsets of size k+1, at least one is false:
    negateAll(Lits,NLits), 
    K1 is K+1,    subsetOfSize(K1,NLits,Clause), writeClause(Clause),fail.
atMost(_,_).

atLeast(K,Lits):-  % l1+...+ln >= k: in all subsets of size n-k+1, at least one is true:
    length(Lits,N),
    K1 is N-K+1,  subsetOfSize(K1, Lits,Clause), writeClause(Clause),fail.
atLeast(_,_).

negateAll( [], [] ).
negateAll( [Lit|Lits], [NLit|NLits] ):- negate(Lit,NLit), negateAll( Lits, NLits ),!.

negate(\+Lit,  Lit):-!.
negate(  Lit,\+Lit):-!.

subsetOfSize(0,_,[]):-!.
subsetOfSize(N,[X|L],[X|S]):- N1 is N-1, length(L,Leng), Leng>=N1, subsetOfSize(N1,L,S).
subsetOfSize(N,[_|L],   S ):-            length(L,Leng), Leng>=N,  subsetOfSize( N,L,S).

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.

% Express that Var is equivalent to the conjunction of Lits:
expressAnd( Var, Lits ):- negate(Var,NVar), member(Lit,Lits),  writeClause([ NVar, Lit ]), fail.
expressAnd( Var, Lits ):- negateAll(Lits,NLits), writeClause([ Var | NLits ]),!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

