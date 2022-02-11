/*
Every professor wants to be on the Athlectics Committee -- free tickets to your favorite event!
To keep the committee from becoming too cliquish, the college forbids anyone from serving who has 3 or more friends on the committee.
That's OK, because if you have 3 or more friends on the committee, you get free tickets to your favorite event!
Can the committee be chosen so that no one on it has 3 friends on the committee, but everyone _off_ the committee has 3 friends on it?

Assumptions: 
- friends is a symmetric relation (i is a friend of j -> j is a friend of i)
- a person is not a friend of him/herself
- the last constraint means that everyone _off_ the committee has _exactly 3 friends in it.
- there must be at least one person in the committe and one person outside it
*/

include "globals.mzn"; 

int: n = 4; % number of professors

array[1..n, 1..n] of var 0..1: friends; % friends[i,j]=1: i is friend of j, friends[i,j]=0: i is not a friend of j
array[1..n] of var 0..1: x; % x[i]=1: i is in the committee

solve satisfy;


constraint
   sum(x) > 0 % at least one person is in the committee
   
  /\ sum(x) < n % at least one person is outside the committee
  
  %everyone in the committee must have less than three friends in it
 /\ forall(i in 1..n) (
    if x[i] = 1 then 
       sum([friends[i,j]*x[j] | j in 1..n]) < 3 
  %everyone out the committee must have exactly three friends in it to have free tickets
    else 
       sum([friends[i,j]*x[j] | j in 1..n]) = 3 
    endif

    /\
    friends[i,i] = 0 % a person is not a friend of him/herself
    /\
    % symmetry of friends
    forall(j in 1..n) (
      friends[i,j] = friends[j,i]
    )
  )


;


%Output that shows: index of professor, if he's in the committee or not, his friends. At the end a list of the committee is shown
output 
[ if j = 1 then "\n" ++ show_int(2,i) ++ " (" ++ show(x[i]) ++ "): " else " " endif ++
  show(friends[i,j])
  | i,j in 1..n  
]
++
[
  "\nx: \(x)\n",
]
;
