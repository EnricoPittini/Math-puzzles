/*
The aim of this exercise is to solve the math puzzle game called Magic Hexagon.
A magic hexagon of order n is an arrangement of close-packed hexagons containing the numbers 1, 2, ..., H_(n-1), where H_n is the nth hex number such that the numbers along 
each straight line add up to the same sum. In the magic hexagon of order n=3, each line (those of lengths 3, 4, and 5) adds up to 38.

For visual reason the magic hexagon is defined as follows, using letters as its elements.

        A,B,C
       D,E,F,G
      H,I,J,K,L
       M,N,O,P
        Q,R,S
*/



include "globals.mzn";
 
% Parameters

% Elements of the order 3 hexagon are integers in the range of 1..19 (being 19 the third hex number)
set of int: N = 1..19;

% Magic number, being the sum of all the hexagon elements belonging to a line at every direction, is 38 in case of hexagon of order 3
int: magic_number=38;

% Elements of the hexagon defined as integers in range N previously defined
var N: a;
var N: b;
var N: c;
var N: d;
var N: e;
var N: f;
var N: g;
var N: h;
var N: i;
var N: j;
var N: k;
var N: l;
var N: m;
var N: n;
var N: o;
var N: p;
var N: q;
var N: r;
var N: s;
 
% 1D array defining the entire magic hexagon
array[N] of var N: elements = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s];
 

% Solving the problem

% Solving the CSP problem
solve satisfy;


% Constraints

% Constraint on uniqueness of all the elements of the hexagon
constraint all_different(elements);
 
% Constraints that check that every line of the hexagon sums up to the magic number
% Horizontal sum
constraint 
  a + b + c =  magic_number /\
  d + e + f + g =  magic_number /\
  h + i + j + k + l =  magic_number /\
  m + n + o + p =  magic_number /\
  q + r + s =  magic_number;

% Main diagonal sum
constraint
  c + g + l =  magic_number /\
  b + f + k + p =  magic_number /\
  a + e + j + o + s =  magic_number /\
  d + i + n + r =  magic_number /\
  h + m + q =  magic_number;
  
% Anti diagonal sum
constraint
  a + d + h =  magic_number /\
  b + e + i + m =  magic_number /\
  c + f + j + n + q =  magic_number /\
  g + k + o + r =  magic_number /\
  l + p + s =  magic_number;
 

% Output

% First output, elements of the hexagon are shown in an array style
output [ show(elements)];
  
% Second output, elements of the hexagon are shown in an hexagon style
output
[
   "\n",
   "\n",
   "            \(elements[1])    \(elements[2])    \(elements[3]) \n",
   "        \(elements[4])     \(elements[5])     \(elements[6])     \(elements[7])\n",
   "     \(elements[8])     \(elements[9])     \(elements[10])     \(elements[11])    \(elements[12])\n",
   "        \(elements[13])     \(elements[14])     \(elements[15])     \(elements[16])\n",
   "            \(elements[17])    \(elements[18])    \(elements[19]) \n",
   "\n",

];
