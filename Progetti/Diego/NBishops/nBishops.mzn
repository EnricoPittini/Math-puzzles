include "alldifferent_except_0.mzn";

enum Coordinates = {r,c};
int: n = 5;
% Up to how many bishops we try to place
int: maxBishops=8;

% Coordinates of the bishops, (0,0) means no bishop
array[1..maxBishops,Coordinates] of var 0..n:bishops;

% Tuples (positions) have to be different
constraint alldifferent_except_0([ bishops[i,r]*n+bishops[i,c] | i in 1..maxBishops]);

% Neither or both of the coordinates have to be 0
constraint forall(k in 1..maxBishops)(bishops[k,r]!=0<->bishops[k,c]!=0);

% Non attacking constraint
predicate notAttacking(var int:r1, var int:c1, var int:r2, var int:c2)= abs(r1-r2) != abs(c1-c2);
constraint forall(i in 1..maxBishops, j in i+1..maxBishops)(
  if (bishops[i,r] != 0 /\ bishops[j,r] != 0) then notAttacking(bishops[i,r], bishops[i,c], bishops[j,r], bishops[j,c]) endif

);

% Minize the number of unset bishops->maximize the number of bishops
var int: count0;
constraint count_eq([ bishops[i,r]*n+bishops[i,c] | i in 1..maxBishops], 0, count0);

% Minizinc is horrendous at output data manipulation and visualization, comment this and part of the output out to actually find a solution in reasonable time
/*
% To help when visualizing
array[1..n,1..n] of var 0..1: board; 
constraint forall(k in 1..maxBishops)(bishops[k,c]!=0 -> board[bishops[k,r],bishops[k,c]]= 1);
*/

solve minimize count0;
output
["Count:\(maxBishops-count0)\n"]++
["(\(bishops[i,r]),\(bishops[i,c]))\n" | i in 1..maxBishops]
/*
++
[if j != n+1 then" __" else "\n" endif | j in 1..n+1]++
[if i mod 2 == 0 
then concat([if j != n+1 then "|"++ show(board[i div 2, j]) ++" " else "|\n" endif | j in 1..n+1])
else concat([if j != n+1 then"|__" else "|\n" endif | j in 1..n+1])
endif | i in 2..n*2+1]
*/
;