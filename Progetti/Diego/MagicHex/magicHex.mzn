include "alldifferent_except_0.mzn";
int: order = 3;
int: cols = order*2-1;
int: rows = order*2-1;
int: hexNumber = 3*(order-1)^2+3*(order-1)+1;
%int: magicC = (9*(order^4-2*order^3+2*order^2-order)+2)div(2*(2*order-1)); %It's actually a normal division
var int: magicC;
array[1..rows,1..cols] of var 0..hexNumber:hex;


% Set values outside hex as 0
constraint forall(i in 1..rows)(forall(j in 1..cols)(
  if i<=cols-abs(j-order) then hex[i,j]!=0 else hex[i,j]=0 endif
));

constraint alldifferent_except_0(hex);

% Column costraints
constraint forall(k in 1..cols)(sum(hex[..,k])=magicC);

% Oblique constraints
% From left, upper oblique 
constraint forall(k in 1..order)(
  sum([hex[k-max(j-order,0),j] | j in 1..order+k-1])=magicC
);
% From left, lower oblique
constraint forall(k in 1..order)(
  sum([hex[min((j+k-1),order+k-1),j] | j in 1..(order*2-k)])=magicC
);
% From right, upper oblique
constraint forall(k in 1..order)(
  sum([hex[k+min(j-order,0),j] | j in (order+1-k)..cols])=magicC
);
% From right, lower oblique
constraint forall(k in 1..order)(
  sum([hex[min((order*2)-j+k-1,order+k-1),j] | j in k..cols])=magicC
);



solve satisfy;
% Trying to generalize the 'pixel art' is harder than the entire exercise
output[ "The magic constant is \(magicC)\n"++
"       __     \n"++
"    __/"++format(2,hex[1,3])++"\\__   \n"++
" __/"++format(2,hex[1,2])++"\\__/"++format(2,hex[1,4])++"\\__ \n"++
"/"++format(2,hex[1,1])++"\\__/"++format(2,hex[2,3])++"\\__/"++format(2,hex[1,5])++"\\\n"++
"\\__/"++format(2,hex[2,2])++"\\__/"++format(2,hex[2,4])++"\\__/\n"++
"/"++format(2,hex[2,1])++"\\__/"++format(2,hex[3,3])++"\\__/"++format(2,hex[2,5])++"\\\n"++
"\\__/"++format(2,hex[3,2])++"\\__/"++format(2,hex[3,4])++"\\__/\n"++
"/"++format(2,hex[3,1])++"\\__/"++format(2,hex[4,3])++"\\__/"++format(2,hex[3,5])++"\\\n"++
"\\__/"++format(2,hex[4,2])++"\\__/"++format(2,hex[4,4])++"\\__/\n"++
"   \\__/"++format(2,hex[5,3])++"\\__/  \n"++
"      \\__/    \n"
];
