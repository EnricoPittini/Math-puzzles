int: sides = 6;
enum Color = {Red, Blue};

% A vertex of the polygon can be assigned a number between 0 and 1 and a color between Red and Blue
array[1..sides] of var Color: colors;
array[1..sides] of var 0..1: numbers;

% Successive vertexes have to have equal color or number
constraint forall(i in 1..sides-1)( colors[i]=colors[i+1] \/ numbers[i]=numbers[i+1]);
constraint colors[sides]=colors[1] \/ numbers[sides]=numbers[1];

% Then you have to tell the solver to count the number of solutions
solve satisfy;