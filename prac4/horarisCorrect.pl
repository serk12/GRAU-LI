:-include(entradaHoraris).
:-include(displaySol).
:-dynamic(varNumber/3).
symbolicOutput(0). % set to 1 to see symbolic output only; 0 otherwise.



%%%%%% Some helpful definitions to make the code cleaner:
day(D)               :- between(1,5,D).
hour(H)              :- between(1,12,H).
year(Y)              :- numYears(N), between(1,N,Y).
course(C)            :- numCourses(N), between(1,N,C).
teacher(T)           :- numTeachers(N), between(1,N,T).
room(R)              :- numRooms(N), between(1,N,R).
lectureOfCourse(C,L) :- course(_,C,N,_,_), between(1,N,L).
courseOfYear(Y,C)    :- course(Y,C,_,_,_).

slot(S)                 :- between(1,60,S).
slotOfDay(D,S)          :- hour(H), S is (D-1)*12 + H.
morningSlotOfDay(D,S)   :- between(1,6,H),  S is (D-1)*12 + H.
afternoonSlotOfDay(D,S) :- between(7,12,H), S is (D-1)*12 + H.

%%%%%%  Variables: It is mandatory to use these variables!
% ct-C-T course-teacher OBLIGATORIA
% cr-C-R course-rooms OBLIGATORIA
% cls-C-L-S course-lecture-slot OBLIGATORIA
% cs-C-S course-slot
% ts-T-S teacher-slot
% ys-Y-S year-slot
% 
% Exam 1:
% tys-Y-S theory year-slot
% 
% Exam 2:
% ydr-Y-D-R year-day-room

writeClauses:- 
    oneSlotPerLecture,
    oneTeacherPerCourse,
    oneRoomPerCourse,
    atMostOneLecturePerDay,
    defineCourseSlot,
    defineTeacherSlot,
    defineYearSlot,
    noOverlappingSameYear,
    noOverlappingSameTeacher,
    noOverlappingSameRoom,
    teachersMorning, teachersAfternoon,
    yearMorning, yearAfternoon,
    max5HoursDay,
    % Examen 1:
    %   defineTheory,
    %   noTwoConsecutiveTheories,
    % Examen 2:
    %   defineYearDayRoom,
    %   eachYearAtMost2RoomsPerDay,
    true.


%%%%% Solucion de la practica:

oneSlotPerLecture:- course(C), lectureOfCourse(C,L), findall(cls-C-L-S,slot(S),Lits), exactly(1,Lits), fail.
oneSlotPerLecture.

oneTeacherPerCourse:- course(C), findall(ct-C-T,(course(_,C,_,_,LT),member(T,LT)),Lits), exactly(1,Lits), fail.
oneTeacherPerCourse.

oneRoomPerCourse:- course(C), findall(cr-C-R,(course(_,C,_,LR,_),member(R,LR)),Lits), exactly(1,Lits), fail.
oneRoomPerCourse.

atMostOneLecturePerDay:- 
    course(C), 
    day(D), 
    lectureOfCourse(C,L1), 
    lectureOfCourse(C,L2), L1 < L2, 
    slotOfDay(D,S1), 
    slotOfDay(D,S2),  
    writeClause([\+cls-C-L1-S1,\+cls-C-L2-S2]), fail.
atMostOneLecturePerDay.

defineCourseSlot:- course(C), lectureOfCourse(C,L), slot(S), writeClause([\+cls-C-L-S,cs-C-S]), fail.
defineCourseSlot.

defineTeacherSlot:- course(C), slot(S), teacher(T), writeClause([\+cs-C-S,\+ct-C-T,ts-T-S]),
debug([\+cs-C-S,\+ct-C-T,ts-T-S]),
fail.
defineTeacherSlot.

defineYearSlot:- year(Y), courseOfYear(Y,C), slot(S), writeClause([\+cs-C-S,ys-Y-S]), fail.
defineYearSlot.

noOverlappingSameYear:- year(Y), courseOfYear(Y,C1), courseOfYear(Y,C2), C1 < C2, slot(S), 
    writeClause([\+cs-C1-S,\+cs-C2-S]), fail.    
noOverlappingSameYear.

noOverlappingSameTeacher:- course(C1), course(C2), C1 < C2, teacher(T), slot(S), 
    writeClause([\+ct-C1-T,\+ct-C2-T,\+cs-C1-S,\+cs-C2-S]), fail.
noOverlappingSameTeacher.

noOverlappingSameRoom:- course(C1), course(C2), C1 < C2, room(R), slot(S), 
    writeClause([\+cr-C1-R,\+cr-C2-R,\+cs-C1-S,\+cs-C2-S]), fail.
noOverlappingSameRoom.

yearMorning:- year(Y,morning), courseOfYear(Y,C), day(D), afternoonSlotOfDay(D,S), writeClause([\+cs-C-S]), fail.
yearMorning.

yearAfternoon:- year(Y,afternoon), courseOfYear(Y,C), day(D), morningSlotOfDay(D,S), writeClause([\+cs-C-S]), fail.
yearAfternoon.

max5HoursDay:- year(Y,morning), day(D), findall(\+ys-Y-S,morningSlotOfDay(D,S),L), writeClause(L), fail.
max5HoursDay:- year(Y,afternoon), day(D), findall(\+ys-Y-S,afternoonSlotOfDay(D,S),L), writeClause(L), fail.
max5HoursDay.

teachersMorning:- teacher(T,morning), day(D), afternoonSlotOfDay(D,S), writeClause([\+ts-T-S]), fail.
teachersMorning.

teachersAfternoon:- teacher(T,afternoon), day(D), morningSlotOfDay(D,S), writeClause([\+ts-T-S]), fail.
teachersAfternoon.

% solucion Examen 1:
defineTheory:- theory(TC), member(C,TC), courseOfYear(Y,C), slot(S), writeClause([\+cs-C-S,tys-Y-S]), fail.
defineTheory.

noTwoConsecutiveTheories:- year(Y), day(D), slotOfDay(D,S1), S2 is S1+1, S2 =< (D-1)*12+12, 
    writeClause([\+tys-Y-S1,\+tys-Y-S2]), fail.
noTwoConsecutiveTheories.

% solucion Examen 2:
defineYearDayRoom:- year(Y), courseOfYear(Y,C), day(D), slotOfDay(D,S), room(R), 
    writeClause([\+cr-C-R,\+cs-C-S,ydr-Y-D-R]), fail.
defineYearDayRoom. 

eachYearAtMost2RoomsPerDay:- year(Y), Y = 1, day(D), findall(ydr-Y-D-R,room(R),L), atMost(2,L), fail.   
eachYearAtMost2RoomsPerDay.

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%% show the solution. Here M contains the literals that are true in the model:

displaySol(M):-write(M),nl,fail.
% Solution for displaySol, Exam 1:
%% displaySol(M):-
%%     nl,nl,write('Two consecutive theories'),nl,
%%     year(Y),nl,
%%     write('Year '),write(Y), write(': '),nl,
%%     day(D), nl,
%%     write('\tDay '),write(D), write(': '),    
%%     setof([S1,S2],consecutiveTheories(Y,D,S1,S2,M),L),
%%     write(L),fail.
%% 
%% consecutiveTheories(Y,D,S1,S2,M):-
%%     slotOfDay(D,S1),
%%     S2 is S1 + 1,
%%     S2 =< (D-1)*12 + 12,
%%     theory(LT),
%%     courseOfYear(Y,C1), member(C1,LT),
%%     member(cls-C1-_-S1,M),
%%     courseOfYear(Y,C2), member(C2,LT),
%%     member(cls-C2-_-S2,M).


% Solution for displaySol, Exam 2:
%% displaySol(M):-
%%     write('Rooms per day'),nl,
%%     year(Y),nl,
%%     write('Year '),write(Y), write(': '),nl,
%%     day(D), 
%%     write('\tDay '),write(D), write(': '),
%%     setof(R,roomYearDay(R,Y,D,M),L),write(L),nl,fail.
displaySol(M):-displaySchedule(M),fail.
displaySol(_).
%% 
%% roomYearDay(R,Y,D,M):-
%%     room(R),
%%     courseOfYear(Y,C),
%%     slotOfDay(D,S),
%%     member(cls-C-_-S,M),
%%     member(cr-C-R,M).



%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Express that Var is equivalent to the disjunction of Lits:
expressOr( Var, Lits ):- member(Lit,Lits), negate(Lit,NLit), writeClause([ NLit, Var ]), fail.
expressOr( Var, Lits ):- negate(Var,NVar), writeClause([ NVar | Lits ]),!.


%%%%%% Cardinality constraints on arbitrary sets of literals Lits:

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


%%%%%% main:

main:-  symbolicOutput(1), !, writeClauses, halt.   % print the clauses in symbolic form and halt
main:-  initClauseGeneration,
        tell(clauses), writeClauses, told,          % generate the (numeric) SAT clauses and call the solver
	tell(header),  writeHeader,  told,
	numVars(N), numClauses(C),
	write('Generated '), write(C), write(' clauses over '), write(N), write(' variables. '),nl,
	shell('cat header clauses > infile.cnf',_),
	write('Calling solver....'), nl, 
	shell('picosat -v -o model infile.cnf', Result),  % if sat: Result=10; if unsat: Result=20.
	treatResult(Result),!.

treatResult(20):- write('Unsatisfiable'), nl, halt.
treatResult(10):- write('Solution found: '), nl, see(model), symbolicModel(M), seen, displaySol(M), nl,nl,halt.

initClauseGeneration:-  %initialize all info about variables and clauses:
    retractall(numClauses(   _)), 
    retractall(numVars(      _)), 
    retractall(varNumber(_,_,_)),
    assert(numClauses( 0 )), 
    assert(numVars(    0 )),     !.

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
