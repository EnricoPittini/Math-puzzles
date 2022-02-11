%%%%%%%%%%%%%%%%
% The objective of this exercise is to try and guess the correct solution for a given configuration of the game of Wordle
%
% The program is given a number of already made guesses and their result(the green/yellow/black color in the background of the Wordle grid)
% It then tries to guess the correct solution based on these guesses, by looking into a dictionary of possible solutions
%
% Sample imput:
% 3 guesses have been  made and they were
% WEARY
% PILLS
% VAGUE
%
% The results of these guesses were:
%  WEARY : A and E are in the word
%  PILLS : P is in the correct position and S is in the word
%  VAGUE : A and E are in the correct position and U is in the word
%
% In this case there is only 1 solution in the supplied dictionary: PAUSE

include "words5Sol.dzn";

%%%%%%%%%%%%%%%%
% Utilities

% Use integers instead of chars
int: a =  1; int: b =  2; int: c =  3; int: d =  4; int: e =  5; int: f =  6;
int: g =  7; int: h =  8; int: i =  9; int: j = 10; int: k = 11; int: l = 12;
int: m = 13; int: n = 14; int: o = 15; int: p = 16; int: q = 17; int: r = 18;
int: s = 19; int: t = 20; int: u = 21; int: v = 22; int: w = 23; int: x = 24;
int: y = 25; int: z = 26; 

% Backward mapping just for visualization purposes
int: nLetters = 26;
array[1..nLetters] of string: letters = 
       ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z"];
       
% Visualizing results of previous guesses
array[0..2] of string: resultHint = array1d(0..2,["_","~","^"]);

%%%%%%%%%%%%%%%%
% Parameters

% Number of guesses the user has made
int: nGuesses = 3;

% The guesses themselves
array[1..nGuesses,1..5] of a..z: previousGuesses =
[|w,e,a,r,y
 |p,i,l,l,s
 |v,a,g,u,e|];

% Result of the previous guesses
% If the letter was at the right position -> 2,
% If the letter is in the word -> 1 
% If the letter is not in the word -> 0
array[1..nGuesses,1..5] of 0..2: guessResult =
[|0,1,1,0,0
 |2,0,0,0,1
 |0,2,0,1,2|];

%%%%%%%%%%%%%%%%
% Variables

% The solution as an array of int characters
array[1..5] of var a..z: solution;
% Solution seen as a set of int characters, just to ease the writing of constraints
var set of a..z: solutionAsSet;

%%%%%%%%%%%%%%%%
% Constraints

% Obtain the solution as a set of int characters
constraint solutionAsSet = array2set(solution);

% If a letter is wrong it won't be in the solution array
constraint forall(ii in 1..nGuesses)(forall(jj in 1..5)(
  guessResult[ii,jj] = 0 -> not (previousGuesses[ii,jj] in solutionAsSet)));

% If a letter is in the solution then it will be in the solution array
constraint forall(ii in 1..nGuesses)(forall(jj in 1..5)(
  guessResult[ii,jj] = 1 -> previousGuesses[ii,jj] in solutionAsSet));

% If a letter is in the correct position update the solution array accordingly
constraint forall(ii in 1..nGuesses)(forall(jj in 1..5)(
  guessResult[ii,jj] = 2 -> (solution[jj] = previousGuesses[ii,jj])));

% The solution has to be a valid word (achieved by looking into the dictionary of words)
constraint exists(ii in 1..numWords5)(solution = words5[ii,..]);

%%%%%%%%%%%%%%%%
% Solving and output

solve :: 
  int_search(solution, first_fail, indomain_min) 
  satisfy;

output ["Problem given:\n"]++
[" ___________________\n"]++
[if ii mod 2 == 0 
then concat([if jj != 5+1 then "| "++ (if (ii div 2 <= nGuesses) then letters[previousGuesses[ii div 2,jj]] else " "  endif ) ++ " " else "|\n" endif | jj in 1..5+1])
else concat([if jj != 5+1 then "|_"++ (if (ii div 2 <= nGuesses) then resultHint[guessResult[ii div 2,jj]] else "_"  endif ) ++ "_" else "|\n" endif | jj in 1..5+1])
endif | ii in 2..6*2+1]
++
["Possible solution:\n"]++ [concat([letters[fix(solution[jj])]| jj in 1..5])]

