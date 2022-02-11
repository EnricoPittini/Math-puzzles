/*
A magic hexagon of order n is an arrangement of close-packed hexagons containing the numbers 1, 2, ..., H_(n-1), where H_n is the nth hex number such that the numbers along each straight line add up to the same sum. (Here, the hex numbers are i.e., 1, 7, 19, 37, 61, 91, 127, ...; OEIS A003215). In the above magic hexagon of order n=3, each line (those of lengths 3, 4, and 5) adds up to 38.

It was discovered independently by Ernst von Haselberg in 1887 (Bauch 1990, Hemme 1990), W. Radcliffe in 1895 (Tapson 1987, Hemme 1990, Heinz), H. Lulli (Hendricks, Heinz), Martin Kühl in 1940 (Gardner 1963, 1984; Honsberger 1973), Clifford W. Adams, who worked on the problem from 1910 to 1957 (Gardner 1963, 1984; Honsberger 1973), and Vickers (1958; Trigg 1964).

This problem and the solution have a long history. Adams came across the problem in 1910. He worked on the problem by trial and error and after many years arrived at the solution which he transmitted to M. Gardner, Gardner sent Adams' magic hexagon to Charles W. Trigg, who by mathematical analysis found that it was unique disregarding rotations and reflections (Gardner 1984, p. 24). Adams' result and Trigg's work were written up by Gardner (1963). Trigg (1964) did further research and summarized known results and the history of the problem.

Trigg showed that the magic constant for an order n hexagon would be

 (9(n^4-2n^3+2n^2-n)+2)/(2(2n-1)), 
the first few of which are 1, 28/3, 38, 703/7, 1891/9, 4186/11, ... (OEIS A097361 and A097362), which requires 5/(2n-1) to be an integer for a solution to exist. But this is an integer for only n=1 (the trivial case of a single hexagon) and Adams's n=3

*/



include "globals.mzn";
 
%decision variables
%variables are integers in the range of 1..19
set of int: N = 1..19;
int: magic_number=38;
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
 
%elements of the magic hexagon
array[N] of var N: elements = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s];
 
solve satisfy;
 
constraint
  %every element should be unique
  all_different(elements) /\  
 
  %the sum of each line adds up to the magic number 
  a + b + c =  magic_number /\
  d + e + f + g =  magic_number /\
  h + i + j + k + l =  magic_number /\
  m + n + o + p =  magic_number /\
  q + r + s =  magic_number /\
  a + d + h =  magic_number /\
  b + e + i + m =  magic_number /\
  c + f + j + n + q =  magic_number /\
  g + k + o + r =  magic_number /\
  l + p + s =  magic_number /\
  c + g + l =  magic_number /\
  b + f + k + p =  magic_number /\
  a + e + j + o + s =  magic_number /\
  d + i + n + r =  magic_number /\
  h + m + q =  magic_number 
;
 
%first output, array style
output
[
  show(elements)
  ];
  
%second output, hexagon style
output
[

   "\n",
   "\n",
   "           \(elements[1])   \(elements[2])   \(elements[3]) \n",
   "        \(elements[4])     \(elements[5])     \(elements[6])     \(elements[7])\n",
   "     \(elements[8])      \(elements[9])      \(elements[10])      \(elements[11])     \(elements[12])\n",
   "        \(elements[13])     \(elements[14])     \(elements[15])     \(elements[16])\n",
   "           \(elements[17])   \(elements[18])   \(elements[19]) \n",
   "\n",

];
