/*

A teacher was leading a class of four perfectly logical students. The teacher chose a set S of four integers and gave a different number in S to each student. Then the teacher announced to the class that the numbers in S were four consecutive two-digit positive integers, that some number in S was divisible by 6, and a different number in S was divisible by 7. 
The teacher then asked if any of the students could deduce what S is, but in unison, all of the students replied no.
However, upon hearing that all four students replied no, each student was able to determine the elements of S. Find the sum of all possible values of the greatest element of S. 

*/
int: l = 4;
int: maxSets=26;

% All 4 elements sets which satisfy the constraints
array[1..maxSets, 1..l] of var 10..99: possibleS;
% For each set, whether any of the students can deduce the entire set from the value he was given
array[1..maxSets] of var bool: canDeduce;

% Set creation
% The set must contain successive numbers
constraint forall(k in 1..maxSets)(forall ( i in 1..l-1)(possibleS[k,i+1] = possibleS[k,i]+1));
% The set must contain an element divisible by 6 and one other by 7
constraint forall(k in 1..maxSets)(exists ( i in 1..l)(exists ( j in 1..l) (possibleS[k,i] mod 6  = 0 /\ possibleS[k,j] mod 7  = 0 /\ i != j)));
% Generate all different sets
constraint forall(i in 1..maxSets)(forall(j in 1..maxSets)(
  (i!=j)->(possibleS[i,..]!= possibleS[j,..])
  )
);

% Sorting
constraint forall(i in 1..maxSets-1)(possibleS[i,1]<possibleS[i+1,1]);

% Finding out if at least one student can deduce which is S
% A student can deduce the entire set if the number he received appears only in one set
% Just need to check the next and previous sets
constraint forall(i in 2..maxSets-1)(canDeduce[i] = exists(j in 1..l)(not (possibleS[i,j] in array2set(possibleS[i-1,..])) /\ not (possibleS[i,j] in array2set(possibleS[i+1,..]))));
constraint (canDeduce[1] = exists(j in 1..l)(not (possibleS[1,j] in array2set(possibleS[2,..]))));
constraint (canDeduce[maxSets] = exists(j in 1..l)(not (possibleS[maxSets,j] in array2set(possibleS[maxSets-1,..]))));

% Sum the maximum of those who can deduce, those are all the possible S that satisfy the problem
output [ "\(sum(i in 1..maxSets)(if not canDeduce[i] then possibleS[i,4] else 0 endif));\n"];

solve satisfy;