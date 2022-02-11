include "alldifferent.mzn";

int: n_rows_barrels;
int: n_columns_barrels;

int: n_rows_per_barrel;
int: n_columns_per_barrel;

int: total_rows = n_rows_barrels*n_rows_per_barrel;
int: total_columns = n_columns_barrels*n_columns_per_barrel;

int: n_barrels = n_rows_barrels*n_columns_barrels;

int: max_number;

int: n_recquired_numbers_barrelsTop;
array[1..n_recquired_numbers_barrelsTop] of int: recquired_numbers_barrelsTop;

array[1..total_rows, 1..total_columns] of var 1..max_number: grid;
array[1..n_barrels] of var int: numbers_barrelsTop;

constraint forall(row in index_set_1of2(grid))(alldifferent(row(grid,row)));
constraint forall(col in index_set_2of2(grid))(alldifferent(col(grid,col)));

predicate barrel_all_different(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: barrel) = let{
  int: row_barrels = barrel div n_columns_barrels;
  int: col_barrels = barrel mod n_columns_barrels;
  
  int: starting_row_grid = row_barrels*n_rows_per_barrel+1;
  int: ending_row_grid = starting_row_grid+n_rows_per_barrel-1;
  int: starting_col_grid = col_barrels*n_columns_per_barrel+1;
  int: ending_col_grid = starting_col_grid+n_columns_per_barrel-1;
  
  array[1..n_rows_per_barrel, 1..n_columns_per_barrel] of var 1..max_number: barrel_grid = array2d(1..n_rows_per_barrel, 1..n_columns_per_barrel, grid[starting_row_grid..ending_row_grid, starting_col_grid..ending_col_grid]);
} in alldifferent(barrel_grid);

constraint forall(barrel in 0..n_barrels-1)(barrel_all_different(grid, barrel));

var int: barrel_sum(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: barrel) = let{
  int: row_barrels = barrel div n_columns_barrels;
  int: col_barrels = barrel mod n_columns_barrels;
  
  int: starting_row_grid = row_barrels*n_rows_per_barrel+1;
  int: ending_row_grid = starting_row_grid+n_rows_per_barrel-1;
  int: starting_col_grid = col_barrels*n_columns_per_barrel+1;
  int: ending_col_grid = starting_col_grid+n_columns_per_barrel-1;
  
  array[1..n_rows_per_barrel, 1..n_columns_per_barrel] of var 1..max_number: barrel_grid = array2d(1..n_rows_per_barrel, 1..n_columns_per_barrel, grid[starting_row_grid..ending_row_grid, starting_col_grid..ending_col_grid]);
} in sum(barrel_grid);

constraint forall(barrel in 0..n_barrels-1)(numbers_barrelsTop[barrel+1]=barrel_sum(grid, barrel));

constraint alldifferent(numbers_barrelsTop);

constraint forall(i in 1..n_recquired_numbers_barrelsTop)(has_element(recquired_numbers_barrelsTop[i], numbers_barrelsTop));

solve satisfy;

string: print_barrel_row(array[1..total_rows, 1..total_columns] of var 1..max_number: grid, int: row_grid, int: col_barrels) = let{
  int: starting_col_grid = col_barrels*n_columns_per_barrel+1;
  int: ending_col_grid = col_barrels*n_columns_per_barrel+n_columns_per_barrel;
  
  int: max_col_grid = n_columns_barrels*n_columns_per_barrel;
  
  bool: ending_barrel_row = row_grid mod n_rows_per_barrel = 0;
} in
  if ((ending_col_grid = max_col_grid) /\ ending_barrel_row) then "\(row(grid,row_grid)[starting_col_grid..ending_col_grid])\n\n" 
  else 
    (if (ending_col_grid = max_col_grid) then "\(row(grid,row_grid)[starting_col_grid..ending_col_grid])\n" 
    else ("\(row(grid,row_grid)[starting_col_grid..ending_col_grid])" ) 
    endif) 
  endif ;

%output ["\(row(grid,r))\n" | r in index_set_1of2(grid)] ++ ["\n"] ++ [show(numbers_barrelsTop)] ;
%output [if ((c = n_columns_barrels*n_columns_per_barrel) /\ (r mod n_rows_per_barrel = 0)) then "\(grid[r,c])\n\n" else (if (c = n_columns_barrels*n_columns_per_barrel) then "\(grid[r,c])\n" else (if (c mod n_columns_per_barrel = 0) then "\(grid[r,c])\t\t" else "\(grid[r,c]) " endif) endif ) endif | r in index_set_1of2(grid), c in index_set_2of2(grid)] ++ [show(numbers_barrelsTop)] ;
output [ print_barrel_row(grid, row_grid,col_barrels)| row_grid in index_set_1of2(grid), col_barrels in 0..n_columns_barrels-1] ++ ["\n"] ++ [show(numbers_barrelsTop)] ;

