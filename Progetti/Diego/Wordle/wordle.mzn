include "alldifferent.mzn";
include "words5Sol.dzn";
include "increasing.mzn";

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

% Frequency of individual characters
array[1..nLetters] of int: letterFrequency =[
5990,1627,2028,2453,6662,1115,1644,1760,3759,291,1505,3371,1976,2952,4438,2019,112,4158,6665,3295,2511,694,1039,288,2074,434
];
int: normConst = 64860;


% Guesses allowed
int: maxGuess = 4;
% Indexes of the chosen words, as indexes of the entire word dictionary
array[1..maxGuess] of var 1..numWords5: wordChoice;
% Set containing the different letters of each chosen word
array[1..maxGuess] of var set of a..z: wordSet;
% Set containing the different letters between all words
var set of a..z: totalUniqueLetters;


% The words chosen have to be different
constraint all_different(wordChoice);

% Compile a set of letters for each word chosen
constraint forall(ii in 1..maxGuess)(wordSet[ii] = 
array2set(array1d(1..5,[words5[wordChoice[ii],jj]| jj in 1..5]))); % To choose words from the dictionary correctly

% Compile a set of letters over all words
constraint totalUniqueLetters = array_union([wordSet[ii] | ii in 1..maxGuess]);

% To weight a solution also by the letters that it incorporates, not only by the number of them
% A better solution is one which incorporates more widely used letters(so according to their frequency)
function var int:letterScore(var set of a..z: letters) = sum([letterFrequency[ii] | ii in letters]);

% Any permutation of words is a symmetry
constraint symmetry_breaking_constraint(increasing(wordChoice));

solve :: seq_search([
  % Choosing a random index as initialization is much better than going by dictionary order (successive words are similar, which is bad for the search)
  int_search(wordChoice, smallest, indomain_random)] 
  )
  % Try to find the set of words with the highest amount of different letters and with the most frequent letters
  % The normalization constant of the letter frequency is multiplied instead of being divided, this way we only have an integer optimization (should be faster)
  
  maximize card(totalUniqueLetters)*normConst + letterScore(totalUniqueLetters);
  % maximize card(totalUniqueLetters);
  
  
output [ "Number of different letters: \(card(totalUniqueLetters)) \nLetter score: \(letterScore(totalUniqueLetters)/normConst)\nWords:\n"
++
concat([ concat([letters[fix(words5[wordChoice[ii],jj])]| jj in 1..5])++"\n" | ii in 1..maxGuess ])];
