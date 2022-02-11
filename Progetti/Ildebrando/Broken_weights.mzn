/*
A merchant had a forty pound measuring weight that broke into four pieces as the result of a fall. When the pieces were subsequently weighed, it was found that the weight of each piece was a whole number of pounds and that the four pieces could be used to weigh every integral weight between 1 and 40 pounds. What were the weights of the pieces?
Note that since this was a 17th-century merchant, he of course used a balance scale to weigh things. So, for example, he could use a 1-pound weight and a 4-pound weight to weigh a 3-pound object, by placing the 3-pound object and 1-pound weight on one side of the scale, and the 4-pound weight on the other side.
*/

%Generalized version for all the weights from 1 to m


int: m = 40; % total weight

int: n = 4; % number of pieces

array[1..n] of var 1..m: weights; % weights of pieces

array[1..m, 1..n] of var -1..1: x; % possible combinations where -1 means on one side of the scale, 1 on the other and 0 on none of them

solve satisfy;

constraint

        sum(weights) = m

        /\ 
%for each possible total weight from 1 to 40 I sum all the possible 4 weights of pieces that should be equal to the total weight           
        forall(j in 1..m) (
          sum([x[j,i]*weights[i] |i in 1..n]) =j 
        )
;

%nicer output found online

output [
  "W:    "
] ++ 
[
   show(weights[i]) ++ "   " 
   | i in 1..n
] ++ ["\n"] ++
[
  if i = 1 then "\n" ++ show_int(2,j) ++ ": " else " " endif ++
     show_int(3, x[j,i])  
  | j in 1..m, i in 1..n
] ++ 
["\n"];