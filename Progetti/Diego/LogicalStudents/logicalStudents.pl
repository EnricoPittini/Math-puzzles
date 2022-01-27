% Predicate: If N is divisible by D R will be 1, else 0
divBy(N, D, R):- integer(N), integer(D), mod(N,D)=:=0, R = 1.
divBy(N, D, R):- integer(N), integer(D), mod(N,D)=\=0, R = 0.

% Predicate: Check/force a list to have N elements
hasLength([],0).
hasLength([_|O],L):-L2 is L-1,hasLength(O,L2),!.

% Predicate: Generate a list of N successive numbers starting from S
generateList([],0,_).
generateList([F|O],N, S):-
    N2 is N-1,
    S2 is S+1,
    generateList(O,N2,S2), 
    F is S.

% Predicate: check if a set is valid according to the definition of S
% A set S has to have only one number divisible by 6,another one by 7 and must be of length 4
validSet(S):-validSetCheck(S,1, 1),length(S,4).

% Base case of a valid set of any length
validSetCheck([F], BY6, BY7):-
    F>=10, F=< 99, 
    divBy(F,6, R6),
    divBy(F,7, R7), 
    BY6 is R6, 
    (R6=:=R7 -> BY7 is 0 ; BY7 is R7). % If the number is divisible by 6 and 7, count it as if it's only div by 6

validSetCheck([F, S|O], BY6, BY7):- 
    % BY6 and BY7 are respectively the counters for how many numbers in the set are divisible by 6 or 7
    % If a number is divisible by both only BY6 will be increased
    integer(F), F>=10, F=< 99, % Only 2 digit numbers
    S is F+1,	% Adjacent numbers must be successors
    S>=10, S=< 99,
    % Recursive call to other successive sequences
 	validSetCheck([S|O], BY61, BY71),
        
    divBy(F,6, R6), % Check if the first number is divisible by 6 or 7 or both
    divBy(F,7, R7),
    BY6 is R6 + BY61,
    (R6=:=R7 -> BY7 is BY71 ; BY7 is R7 + BY71). %If it's divisible by 6 and 7(or neither) don't increase BY7
    

% Predicate: Generate the entire list of possible sets S
listOfSets([],[]).

listOfSets([[F1|FO] | O], [L1 |LO]):- 
    F1 is L1,hasLength([F1|FO],4), 	% Force [F1|FO] to start with a given element and be of length 4
    validSet([F1|FO]), % If it's possible to build a valid set with the given starting element do  it
    listOfSets(O, LO).

listOfSets(O, [_ |LO]):- 
    listOfSets(O, LO). % Skip invalid starting elements

nsets:-generateList(L,90,10),listOfSets(R,L),!,length(R,X), write('The number of possible sets is:'), write(X).

% Predicate: 

% First position
deductableArray1([S1,SN |O],[L1|LO]):-
    intersection(S1,SN,I),
    length(I,LEN), LEN=:=4, L1 is 0, 
    deductableArray([S1,SN |O],LO).
deductableArray1([S1,SN |O],[L1|LO]):-
    intersection(S1,SN,I),
    length(I,LEN), LEN<4, L1 is 1, 
    deductableArray([S1,SN |O],LO).

% General case
deductableArray([SP,S1,SN |O], [L1|LO]):-
    intersection(SP,S1,I1),
    intersection(S1,SN,I2),
    union(I1,I2,IU),
    length(S1,LEN),length(IU,LENU),
    LEN=:=LENU, L1 is 0,
    deductableArray([S1,SN|O],LO).
deductableArray([SP,S1,SN |O], [L1|LO]):-
    intersection(SP,S1,I1),
    intersection(S1,SN,I2),
    union(I1,I2,IU),
    length(S1,LEN),length(IU,LENU),
    LEN=\=LENU, L1 is 1,
    deductableArray([S1,SN|O],LO).

% Last case
deductableArray([SP, S1],[0]):-
    intersection(SP,S1,I),
    length(I,LEN), LEN=:=4.
deductableArray([SP, S1],[1]):-
    intersection(SP,S1,I),
    length(I,LEN), LEN<4.

% Final sum
sumUp([],[],0).
sumUp([S|SO],[0|DO],SUM):-
    sumUp(SO,DO,S1),nth1(4,S,SE), SUM is S1 + SE.
sumUp([S|SO],[1|DO],SUM):-
    sumUp(SO,DO,S1),SUM is S1.

setSum:-generateList(L,90,10),listOfSets(R,L),!,deductableArray1(R,D),!,sumUp(R,D,FR),write("The sum of possible sets is "), write(FR).