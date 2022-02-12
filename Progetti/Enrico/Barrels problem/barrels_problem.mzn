/* 
  BARRELS PROBLEM

  A grid of barrels is given: grid `n_rows_barrels` x `n_columns_barrels`. The total number of barrels is 
  `n_rows_barrels`*`n_columns_barrels`.
  Each barrel is a grid of cells: grid `n_rows_per_barrel` x `n_columns_per_barrel`.
  Therefore, on the whole, we have a grid of cells which is 
                (`n_rows_barrels`*`n_rows_per_barrel`) x (`n_columns_barrels`*`n_columns_per_barrel`).
  Each cell contains a number, which is in the range 1..`max_number`.
  Each barrel has associated a number, which is the sum of all the elements in that barrel: this number is put on the top of 
  the corresponding barrel.

  Example
  n_rows_barrels=4
  n_columns_barrels=3
  n_rows_per_barrel=3
  n_columns_per_barrel=2
  max_number=14
                          Barrel 0   Barrel 1   Barrel 2
                          Sum 45     Sum 57     Sum 49
                          [3, 10]    [8, 11]    [9, 12]
                          [4, 9]     [12, 10]   [6, 11]
                          [11, 8]    [7, 9]     [1, 10]

                          Barrel 3   Barrel 4   Barrel 5
                          Sum 38     Sum 37     Sum 50
                          [1, 7]     [6, 8]     [11, 9]
                          [5, 4]     [11, 7]    [10, 8]
                          [9, 12]    [3, 2]     [5, 7]

                          Barrel 6   Barrel 7   Barrel 8
                          Sum 46     Sum 41     Sum 30
                          [7, 11]    [1, 12]    [3, 6]
                          [12, 2]    [13, 6]    [8, 4]
                          [8, 6]     [5, 4]     [7, 2]

                          Barrel 9   Barrel 10   Barrel 11
                          Sum 27     Sum 24     Sum 28
                          [10, 3]    [2, 1]     [13, 5]
                          [2, 1]     [9, 5]     [4, 3]
                          [6, 5]     [4, 3]     [2, 1]
  There are 4*3=12 barrels in total, numerated from 0 up to 11.
  On the whole, the grid of cells is a grid (4*3) x (3*2) = 12 x 6. 12 rows and 6 columns.

  In building such structure, there are some constraints.
      1. Each row of the grid of cells must contain all different numbers.
      2. Each column of the grid of cells must contain all different numbers.
      3. All the numbers on top of the barrels must be different numbers.
  
  In addition, the user can specify that some specific numbers must be present on top of the barrels.
  
  
  
  PARAMETERS
  ---------------
    - int: n_rows_barrels
      Number of rows in the grid of barrels.
    - int: n_columns_barrels
      Number of columns in the grid of barrels.

    - int: n_rows_per_barrel
      Number of rows in each barrel.
    - int: n_columns_per_barrel
      Number of columns in each barrel.

    - int: max_number
      Maximum number that can be present in each cell.

    - int: n_recquired_numbers_barrelsTop
      Number of recquired numbers on the top of the barrels. 
    - array[1..n_divisors] of int: recquired_numbers_barrelsTop
      Array containing the recquired numbers on the top of the barrels.   
          
*/


include "alldifferent.mzn";



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARAMETERS
% Number of rows in the grid of barrels
int: n_rows_barrels;
% Number of columns in the grid of barrels
int: n_columns_barrels;

% Number of rows in each barrel
int: n_rows_per_barrel;
% Number of columns in each barrel
int: n_columns_per_barrel;

% Maximum number that can be present in each cell
int: max_number;

% Number of recquired numbers on the top of the barrels
int: n_recquired_numbers_barrelsTop;
% Array containing the recquired numbers on the top of the barrels
array[1..n_recquired_numbers_barrelsTop] of int: recquired_numbers_barrelsTop;

% Total number of rows in the grid of cells
int: total_rows = n_rows_barrels*n_rows_per_barrel;
% Total number of columns in the grid of cells
int: total_columns = n_columns_barrels*n_columns_per_barrel;

% Number of barrels
int: n_barrels = n_rows_barrels*n_columns_barrels;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VARIABLES
% Grid of cells
array[1..total_rows, 1..total_columns] of var 1..max_number: grid;

% Numbers on the top of the barrels
array[1..n_barrels] of var int: numbers_barrelsTop;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER-DEFINED FUNCTIONS

/*
*  get_barrel: returns, from the given grid of cells, the barrel with the specified index
*  
*  A barrel is a grid of cells: grid `n_rows_per_barrel` x `n_columns_per_barrel`.
*
*  Input
*  ------------
*      - array[1..total_rows, 1..total_columns] of var 1..max_number: grid
*        Grid of cells.
*      - int: barrel_idx
*        Index of the desired barrel.
*
*  Output
*  ------------
*       array[1..n_rows_per_barrel, 1..n_columns_per_barrel]: barrel
*/
array[1..n_rows_per_barrel, 1..n_columns_per_barrel] of var 1..max_number: get_barrel(
  array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: barrel_idx) = let{
  % Row index of that barrel
  int: row_barrels = barrel_idx div n_columns_barrels;
  % Column index of that barrel
  int: col_barrels = barrel_idx mod n_columns_barrels;
  
  % Starting row of that barrel in the grid of cells 
  int: starting_row_grid = row_barrels*n_rows_per_barrel+1;
  % Ending row of that barrel in the grid of cells 
  int: ending_row_grid = starting_row_grid+n_rows_per_barrel-1;
  % Starting column of that barrel in the grid of cells
  int: starting_col_grid = col_barrels*n_columns_per_barrel+1;
  % Ending column of that barrel in the grid of cells
  int: ending_col_grid = starting_col_grid+n_columns_per_barrel-1;
  
  array[1..n_rows_per_barrel, 1..n_columns_per_barrel] of var 1..max_number: barrel = 
         array2d(1..n_rows_per_barrel, 1..n_columns_per_barrel, 
                 grid[starting_row_grid..ending_row_grid, starting_col_grid..ending_col_grid]);
} 
  in barrel;

/*
*  barrel_sum: returns, from the given grid of cells, the sum of all the elements of the barrel with the specified index
*
*  Input
*  ------------
*      - array[1..total_rows, 1..total_columns] of var 1..max_number: grid
*        Grid of cells.
*      - int: barrel_idx
*        Index of the desired barrel.
*
*  Output
*  ------------
*       var int
*/
var int: barrel_sum(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: barrel_idx) = 
  sum(get_barrel(grid, barrel_idx));



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER-DEFINED PREDICATES

/*
*  barrel_all_different: the elements of the barrel with the specified index, from the given grid of cells, are all different
*
*  Input
*  ------------
*      - array[1..total_rows, 1..total_columns] of var 1..max_number: grid
*        Grid of cells.
*      - int: barrel_idx
*        Index of the desired barrel.
*/
predicate barrel_all_different(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: barrel_idx) = 
  alldifferent(get_barrel(grid, barrel_idx));



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSTRAINTS

% Each row of the grid of cells has all different elements 
constraint forall(row in index_set_1of2(grid))(alldifferent(row(grid,row)));
% Each columns of the grid of cells has all different elements 
constraint forall(col in index_set_2of2(grid))(alldifferent(col(grid,col)));

% Each barrel has all different elements
constraint forall(barrel_idx in 0..n_barrels-1)(barrel_all_different(grid, barrel_idx));

% Each number on the top of a barrel is equal to the sum of all the elements of the corresponding barrel
constraint forall(barrel_idx in 0..n_barrels-1)(numbers_barrelsTop[barrel_idx+1]=barrel_sum(grid, barrel_idx));

% All the numbers on the top of the barrels are different
constraint alldifferent(numbers_barrelsTop);

% All the numbers specified by the users are on the top of the barrels
constraint forall(i in 1..n_recquired_numbers_barrelsTop)(has_element(recquired_numbers_barrelsTop[i], numbers_barrelsTop));

solve satisfy;



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OUTPUT

/*
*  print_barrel_row: returns, from the given grid of cells, the string representing the specified row of the specified barrel
*
*  The string representation of this barrol row must take into account the position of the barrel.
*  Basically:
*     - if the barrel is the last barrel on the right and the row is the last row of the barrel, there must be two 
*       terminating new lines (i.e. '\n\n');
*     - if the barrel is the last barrel on the right, but the row is not the last of the barrel, there must be only one 
*       terminating new line (i.e. '\n');
*     - in all the other cases, no new line is needed.
*
*   This function is used for the output.
*
*  Input
*  ------------
*      - array[1..total_rows, 1..total_columns] of var 1..max_number: grid
*        Grid of cells.
*      - int: row_grid
*        Row of interest of the grid
*      - int: col_barrels
*        Column of the barrel of interest
*
*  Output
*  ------------
*       string: string representation of the specified row of the specified barrel
*/
string: print_barrel_row(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: row_grid, int: col_barrels) 
= let{
  % Starting column of that barrel in the grid of cells
  int: starting_col_grid = col_barrels*n_columns_per_barrel+1;
  % Ending column of that barrel in the grid of cells
  int: ending_col_grid = col_barrels*n_columns_per_barrel+n_columns_per_barrel;
  
  % Maximum column of the grid of cells
  int: max_col_grid = n_columns_barrels*n_columns_per_barrel;
  
  % Boolean flag which says if the row is the last row of the barrel
  bool: ending_barrel_row = row_grid mod n_rows_per_barrel = 0;
} in
  % The barrel is the last barrel on the right and the row is the last row of the barrel
  if ((ending_col_grid = max_col_grid) /\ ending_barrel_row) then "\(row(grid,row_grid)[starting_col_grid..ending_col_grid])\n\n" 
  else 
    % The barrel is the last barrel on the right, but the row is not the last of the barrel
    (if (ending_col_grid = max_col_grid) then "\(row(grid,row_grid)[starting_col_grid..ending_col_grid])\n" 
    % All the other cases
    else ("\(row(grid,row_grid)[starting_col_grid..ending_col_grid])" ) 
    endif) 
  endif ;

%output ["\(row(grid,r))\n" | r in index_set_1of2(grid)] ++ ["\n"] ++ [show(numbers_barrelsTop)] ;
%output [if ((c = n_columns_barrels*n_columns_per_barrel) /\ (r mod n_rows_per_barrel = 0)) then "\(grid[r,c])\n\n" else (if (c = n_columns_barrels*n_columns_per_barrel) then "\(grid[r,c])\n" else (if (c mod n_columns_per_barrel = 0) then "\(grid[r,c])\t\t" else "\(grid[r,c]) " endif) endif ) endif | r in index_set_1of2(grid), c in index_set_2of2(grid)] ++ [show(numbers_barrelsTop)] ;
output [ print_barrel_row(grid, row_grid,col_barrels)| row_grid in index_set_1of2(grid), col_barrels in 0..n_columns_barrels-1] ++ ["\n"] ++ [show(numbers_barrelsTop)] ;

