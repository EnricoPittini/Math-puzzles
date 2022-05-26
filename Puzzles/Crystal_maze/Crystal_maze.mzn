/*
 On the wall was written a clue: "No consecutive letters in adjacent circles". The letters A to H were printed on circular plates which could be fitted onto each circle.
 
      ?  ?
  ?   A  H   ?
      ?  ? 
      
*/

include "globals.mzn"; 
/*
represented with numbers the problem is the following
       2 3
    1  4 5  6 
       7 8
*/

%conversion from letters to numbers in order to check distances 
int: A = 1;
int: B = 2;
int: C = 3;
int: D = 4;
int: E = 5;
int: F = 6;
int: G = 7;
int: H = 8;

int: n = 8; % number of letters/variables
int: num_connections = 16; %number of connections between circles
array[1..num_connections,1..2] of int: connections; %2D array of all possible connections

array[1..n] of int: hints; %generalization of possible hints that can be given at the beginning of the problem in order to solve it

array[1..n] of string: s = ["A","B","C","D","E","F","G","H"]; %string that will substitute numbers later in the output

%example of possible hints, this is the base case of the puzzle where A is in position 4 and H in position 5

hints = [
   0, % 1
   0, % 2
   0, % 3
   A, % 4
   H, % 5
   0, % 6
   0, % 7
   0, % 8
  
];

%define adjacent circles
connections = array2d(1..num_connections, 1..2,
[
  1,2,
  1,4,
  1,7,
  2,3,
  2,4,
  2,5,
  3,5,
  3,6,
  4,5,
  4,7,
  4,8,
  5,6,
  5,7,
  5,8,
  6,8,
  7,8  
]);


% decision variables
array[1..n] of var 1..n: x;

solve satisfy;


constraint

  all_different(x) /\

  forall(i in 1..num_connections) (
     abs(x[connections[i,1]] - x[connections[i,2]]) > 1 %no consecutive letters in adjacent circles
  )

  /\
  %substitution of hints inside the maze
  forall(i in 1..n) (
    if hints[i] > 0 then
       x[i] = hints[i]
    else
      true
    endif
  )

;

output [

 show(x), "\n",
 show([s[fix(x[i])] | i in 1..n]), "\n",
 
]
++
%output with numbers
[
   "\n",
   "       \(x[2])    \(x[3])\n",
   "  \(x[1])    \(x[4])    \(x[5])    \(x[6])\n",
   "       \(x[7])    \(x[8])\n",
   "\n",
]
++
%output with letters as strings substituted to the respective number
[
   "\n",
   "      \(s[fix(x[2])])    \(s[fix(x[3])])\n",
   " \(s[fix(x[1])])  \(s[fix(x[4])])    \(s[fix(x[5])])    \(s[fix(x[6])])\n",
   "      \(s[fix(x[7])])    \(s[fix(x[8])])\n",
   "\n",
]
;