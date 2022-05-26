/*
The aim of this exercise is to solve the CSP problem of the Athletics Committee, proposed by research mathematician Peter Wrinkler during the 13th Gathering 4 Gardner conference.
It states as follows:                            
"Every professor wants to be on the Athlectics Committee -- free tickets to your favorite event!
To keep the committee from becoming too cliquish, the college forbids anyone from serving who has 3 or more friends on the committee.
That's OK, because if you have 3 or more friends on the committee, you get free tickets to your favorite event!
Can the committee be chosen so that no one on it has 3 friends on the committee, but everyone _off_ the committee has 3 friends on it?"
*/

include "globals.mzn"; 

% Parameters 

% Number of professors
int: n = 4; 

% 2D Array that shows the friend relationship between every professor, in particular friends[i,j]=1 means that i is a friend of j, while friends[i,j]=0    % means that i is not a friend of j
array[1..n, 1..n] of var 0..1: friends; 

% 1D Array that takes into consideration the members of the committee, in particular x[i]=1 means that i is in the committee, while x[i]=0 means that i is % not in the committee
array[1..n] of var 0..1: x; 


% Solving the problem

% Solving the CSP problem
solve satisfy;

% Solving the COP problem by maximizing (or minimizing) the number of members of the committee
%solve maximize sum(x); 
%solve minimize sum(x);


%Constraints

% Extra assumption in order to avoid the committee being to crowded by imposing that the committee must be less than half of the professors if the number % of professors is greater than 6 
constraint if n>6 then sum(x) < n/2 endif;

% Constraint that checks that at least one person is in the committee
constraint sum(x) > 0; 
  
% Constraint that checks that at least one person is outside the committee
constraint sum(x) < n; 
  
% Constraint that checks that everyone in the committee has less than three friends in it
constraint forall(i in 1..n) (
    if x[i] = 1 then 
       sum([friends[i,j]*x[j] | j in 1..n]) < 3 
       
% and that everyone outside the committee has exactly three friends in it in order to have free tickets
    else 
       sum([friends[i,j]*x[j] | j in 1..n]) = 3 
    endif);
    
% Constraint on anti reflexivity of friend relation (i.e. a person is not a friend of himself)
constraint forall(i in 1..n) (friends[i,i] = 0);

% Constraint on symmetry of friend relation 
constraint forall(i in 1..n)(
            forall(j in 1..n) (friends[i,j] = friends[j,i])
            );


%Output


% In the output are shown in the following order: index of each professor, if he's in the committee or not, list of all his friends. 
% At the end also a list of the entire committee is shown.
output [ if j = 1 then "\n" ++ show_int(2,i) ++ " (" ++ show(x[i]) ++ "): " else " " endif ++ show(friends[i,j]) | i,j in 1..n ] ++ [ "\nx: \(x)\n",];
