int: l = 4;
int: maxSets = (99-10+1-(l-1));

% All 4 elements sets of successive elements
array[1..maxSets, 1..l] of var 10..99: successiveSets;
% Array that says whether a set of successive elements satisfies the constraints
array[1..maxSets] of var bool: correctSet;

constraint successiveSets[1,1] = 10;
constraint forall(k in 2..maxSets)(successiveSets[k,1] = successiveSets[k-1,1]+1);
constraint forall(k in 1..maxSets)(forall(i in 2..l)(successiveSets[k,i] = successiveSets[k,i-1]+1));

constraint forall(k in 1..maxSets)(
  if (exists(i in 1..l)(exists(j in 1..l)(successiveSets[k,i] mod 6 = 0 /\ successiveSets[k,j] mod 7 = 0 /\ i!=j))) 
  then correctSet[k]=true 
  else correctSet[k]=false  endif);
  
output [ "Number of total possible sets:\(sum(correctSet));\n"];
