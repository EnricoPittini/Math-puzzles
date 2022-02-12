:- use_module(library(clpfd)).
:- use_rendering(table). % It renders a list of list (i.e. a matrix) as a table

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
  

  
  EXAMPLE OF QUERY
  ---------------
  `Grid_rows` represents the grid of cells: it is a list of list, where each list is a row of the grid of cells.
  `Numbers_barrelsTop` is the list containing the numbers on top of the barrels.
  > barrels_problem(Grid_rows, Numbers_barrelsTop, 4, 3, 3, 2, 14, [45]), maplist(label, Grid_rows), label(Numbers_barrelsTop).
  TRUE  
        Grid_rows = [[1, 2, 3, 4, 5, 6], 
                     [3, 12, 13, 14, 8, 11], 
                     [13, 14, 11, 12, 9, 10], 
                     [2, 1, 4, 3, 6, 5], 
                     [4, 6, 1, 5, 7, 12], 
                     [12, 13, 14, 10, 11, 9], 
                     [5, 3, 2, 1, 4, 7], 
                     [6, 7, 5, 8, 1, 13], 
                     [14, 11, 12, 13, 2, 3], 
                     [7, 4, 6, 2, 3, 1], 
                     [8, 5, 10, 9, 13, 4], 
                     [11, 9, 8, 7, 12, 14]] 
        Numbers_barrelsTop = [45, 57, 49, 38, 37, 50, 46, 41, 30, 44, 42, 47]
    The grid of barrels is a grid 4x3.
    Each barrel is a grid 3x2.
    So, on the whole the grid of cells is a grid (4*3)x(3*2) = 12x6.
    And there are 4*3 = 12 barrels in total.
    The maximum number contained in each cell is 14.
    Finally, the list of recquired numbers on the top of the barrels is [45].

*/




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AUXILIARY PREDICATES



/* sum(L, S): S is the sum of the elements in the list L

   Arguments
   --------------
        - L: list of int
        - S: int
*/
% Base case: the list L is empty L=[]
sum([],0).
% Recursive case: the list L has at least one element L=[H|T]
sum([H|T],S1) :- 
    sum(T,S),  % Recursive call: S is the sum of the elements in the tail T
    S1 #= H+S.  % S1 is equal to the sum H+S



% TODO : reverse order of the arguments?
/* members(L,Es): all the elements in the list Es are contained in list L

   Arguments
   --------------
        - L: list 
        - Es: list
*/
% Base case: the list Es is empty Es=[]
members(_, []).
% Recursive case: the list Es has at least one element Es=[H|T]
members(L, [H|T]) :-
    memberchk(H, L),  % The head H is contained in the list L
    members(L, T).  % Recursive call: all the elements in the tail T are contained in the list L
    


/* len(N, L): N is the length of the list L

   A built-in `length(L,N)` predicate already exists.
   The purpose of `len(N, L)` is simply to have the arguments in the reversed order: in this way it can be used with the 
   `applymap(G,L)` predicate.

   Arguments
   --------------
        - N: int
        - L: list 
*/
len(N,L) :- length(L,N).



/* select_sublist(Start, End, L, L1): L1 is the sublist of the list L consisting in the elements from position Start to
                                      position End (extremes included)

    The counting of the position of the elements is done starting from 1.

   Arguments
   --------------
        - Start: int
          Starting position of the sublist.
          It must be greater or equal than 1.
        - End: int
          Ending position of the sublist.
          It must be less or equal than the length of the list L.
        - L: list
          It is the whole list.
        - L1: list
          It is the sublist of L, from Start to End.
          (extremes included)
*/
% Base case: Start>End.
% In this case, The sublist L1 is the empty list [], for whatever list L 
select_sublist(Start, End, _, []) :- 
    Start #> End.

% Recursive case: 1<Start<=End
% Let the list L be [H|T].
% Since Start>1, we are not interested in the first element H of L.
% We are interested in the sublist of the tail T, starting from Start-1 up to End-1: recursive call on the tail T.
select_sublist(Start, End, [_|T], L1) :- 
    Start #> 1,
    Start #=< End,
    Start1 #= Start-1, End1 #= End-1,
    select_sublist(Start1, End1, T, L1).  % Recusrive call: L1 is the sublist of the tail T of L, from Start-1 up to End-1

% Recursive case: Start=1
% Let the list L be [H|T].
% Since Start=1, we are interested in the first element H of L.
% If L1 is the sublist of the tail T from 1 up to End-1, then the final whole sublist of interest is the list [H|L1]
select_sublist(Start, End, [H|T], [H|L1]) :- 
    Start #= 1,
    Start #=< End,
    End1 #= End-1,
    select_sublist(Start, End1, T, L1).  % Recusrive call: L1 is the sublist of the tail T of L, from 1 (i.e. Start) up to 
                                         % End-1
    % On the whole, the final sublist is [H|L1]



/* select_submatrix(Starting_row, Ending_row, Starting_col, Ending_col, M, M1): 
                    M1 is the submatrix of the matrix M consisting in the elements from row Starting_row up to Ending_row 
                    and from column Starting_col up to Ending_col (extremes included)

   The counting of the rows and columns is done starting from 1.

   A matrix is a list of lists in which the inner lists represent the rows and they have the same length.
   So, the number of inner lists is the number of rows, while the length of each inner list is the number of columns.

   Arguments
   --------------
        - Starting_row: int
          Starting row of the submatrix.
          It must be greater or equal than 1.
        - Ending_row: int
          Ending row of the submatrix.
          It must be less or equal than the number of rows of the matrix M.
        - Starting_col: int
          Starting column of the submatrix.
          It must be greater or equal than 1.
        - Ending_col: int
          Ending column of the submatrix.
          It must be less or equal than the number of columns of the matrix M.        
        - M: list of lists
          It is the whole matrix
        - M1: list of lists
          It is the submatrix of M, from row Starting_row up to Ending_row and from column Starting_col up to Ending_col.
          (extremes included)
*/
select_submatrix(Starting_row, Ending_row, Starting_col, Ending_col, M, M1) :-
    select_sublist(Starting_row, Ending_row, M, M_temp),  % M_temp is the submatrix of M in which there are only the rows 
                                                          % from Starting_row up to Ending_row

    % M1 is the submatrix obtained from M_temp by applying select_sublist(Starting_col, Ending_col) to all the rows in 
    % M_temp.
    % So, M1 is the submatrix of M_temp in which there are only the columns from Starting_col up to Ending_col
    maplist(select_sublist(Starting_col, Ending_col), M_temp, M1). 
    % More precise description. To each row M_temp_row of M_temp, it is applied the predicate 
    %                       select_sublist(Starting_col, Ending_col, M_temp_row, M1_row)
    % where M1_row is the corresponding row of M1. 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BARRELS PROBLEM 


/*
    barrels_problem(Grid_rows, Numbers_barrelsTop, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
                    N_columns_per_barrel, Max_number, Recquired_numbers_barrelsTop) 

    `Grid_rows` is a grid of cells, composed of a grid of barrels `N_rows_barrels`x`N_columns_barrels`, where each barrel
    is a grid `N_rows_per_barrel`x`N_columns_per_barrel`: so, on the whole `Grid_rows` is a grid 
             (`N_rows_barrels`*`N_rows_per_barrel`)x(`N_columns_barrels`*`N_columns_per_barrel`).
    Each cell contains a number which is in the interval 1..`Max_number`.
    Each row, each column and each barrel must contain all different elements.

    `Grid_rows` is a list of list (i.e. matrix), where each inner list is a row.
    The number of inner lists is the number of rows, while the length of each inner list is the number of columns.

    `Numbers_barrelsTop` is the list containing the numbers on top of the barrels: each number on top of a barrel is the sum
    of all the numbers in that barrel.
    This list must contain all different elements.
    `Recquired_numbers_barrelsTop` is the list of the recquired numbers on top of the barrels.

    Arguments
    --------------
        - Grid_rows: list of list of int (matrix)
          Grid of cells.
        - Numbers_barrelsTop: list of int
          List containing the numbers on the top of the barrels.
        - N_rows_barrels: int
          Number of rows in the grid of barrels.
        - N_columns_barrels: int
          Number of columns in the grid of barrels.
        - N_rows_per_barrel: int
          Number of rows in each barrel.
        - N_columns_per_barrel: int
          Number of columns in each barrel.
        - Max_number: int
          Maximum number that can be present in each cell.
        - Recquired_numbers_barrelsTop: list of int
          List containing the recquired numbers on the top of the barrels.

*/
barrels_problem(Grid_rows, Numbers_barrelsTop, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, 
                Max_number, Recquired_numbers_barrelsTop) :-
    Total_rows #= N_rows_barrels*N_rows_per_barrel,  % Total number of rows in the grid of cells
    Total_columns #= N_columns_barrels*N_columns_per_barrel,  % Total number of columns in the grid of cells
    
    length(Grid_rows, Total_rows),  % The number of rows in Grid_rows must be equal to Total_rows
    maplist(len(Total_columns), Grid_rows),  % Each row must have the same length, equal to Total_columns

    append(Grid_rows, Grid),  % Grid is a flat list containing all the elements of Grid_rows
    Grid ins 1..Max_number,  % All the elements must be in the interval 1..Max_number

    maplist(all_distinct, Grid_rows),  % Each row must contain all different elements
    transpose(Grid_rows, Grid_columns),  % Grid_columns is a list of lists in which each inner list is a column of the grid
    maplist(all_distinct, Grid_columns),  % Each column must contain all different elements
    
    % Barrels is the list of all the barrels of the grid.
    % A barrel is a grid of cells, represented as a list of lists (i.e. matrix).
    % So, on the whole, Barrels is a list of lists of lists.
    barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, Barrels),

    % Barrels_flat is still the list of all the barrels of the grid, but now each barrel is represented as a simple list of 
    % all the elements in that barrel.
    % Basically, each barrel now has a flat representation. 
    maplist(append, Barrels, Barrels_flat), 

    % Each barrel must contain all different elements
	maplist(all_distinct, Barrels_flat),
    
    % Each element in Numbers_barrelsTop is the sum of all the elements in the corresponding barrel
    maplist(sum, Barrels_flat, Numbers_barrelsTop),
    
    % Numbers_barrelsTop must contain all different elements
    all_distinct(Numbers_barrelsTop),
    
    % All the elements in Recquired_numbers_barrelsTop must be present in Numbers_barrelsTop
    members(Numbers_barrelsTop, Recquired_numbers_barrelsTop).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BARRELS


/*
    barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, Barrels) 
        Barrels is the list of all the barrels of the grid.
        A barrel is a grid of cells, represented as a list of lists (i.e. matrix).
        So, on the whole, Barrels is a list of lists of lists.

    The barrels are sorted from the top-left barrel (i.e. barrel with index 0) to the bottom-rigth one (i.e. barrel with 
    index N_barrels-1). 

    Arguments
    --------------
        - Grid_rows: list of list of int (matrix)
          Grid of cells.
        - N_rows_barrels: int
          Number of rows in the grid of barrels.
        - N_columns_barrels: int
          Number of columns in the grid of barrels.
        - N_rows_per_barrel: int
          Number of rows in each barrel.
        - N_columns_per_barrel: int
          Number of columns in each barrel.
        - Barrels: list of list of list of int
          List containing all the barrels of the grid.

*/
barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, Barrels) :-
        N_barrels #= N_rows_barrels*N_columns_barrels,  % Number of barrels
        Last_barrel_idx #= N_barrels-1,  % Index of the last barrel
        % Call the auxiliary predicate barrels_AUX, with First_barrel_idx=0
        barrels_AUX(Grid_rows, N_columns_barrels, N_rows_per_barrel, 
            N_columns_per_barrel, 0, Last_barrel_idx, Barrels).



/*
    barrels_AUX(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, First_barrel_idx, Last_barrel_idx,
                Barrels) 
        Barrels is the list of the barrels of the grid, from the barrel with index First_barrel_idx to the barrel with index 
        Last_barrel_idx.
        
    The aim of this auxiliary predicate is to implement the `barrels` predicate.

    Arguments
    --------------
        - Grid_rows: list of list of int (matrix)
          Grid of cells.
        - N_columns_barrels: int
          Number of columns in the grid of barrels.
        - N_rows_per_barrel: int
          Number of rows in each barrel.
        - N_columns_per_barrel: int
          Number of columns in each barrel.
        - First_barrel_idx: int
          Index of the first barrel of interest.
        - Last_barrel_idx: int
          Index of the last barrel of interest.
        - Barrels: list of list of list of int
          List containing all the barrels of the grid.

*/
% Base case: First_barrel_idx > Last_barrel_idx.
% In this case, the list of barrels is an empty list: Barrels=[]
barrels_AUX(_, _, _, _, First_barrel_idx, Last_barrel_idx, []) :- 
    First_barrel_idx #> Last_barrel_idx.

% Recursive case: First_barrel_idx <= Last_barrel_idx
barrels_AUX(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, First_barrel_idx, Last_barrel_idx, 
            [B|Barrels]) :-
    % B is the barrel of the grid with index First_barrel_idx
    barrel(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, First_barrel_idx, B),

    % Recursive call: Barrels is the list of barrels of the grid from the barrel with index First_barrel_idx+1 to the barrel 
    % with index Last_barrel_idx
    First_barrel_idx1 #= First_barrel_idx+1,
    barrels_AUX(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, First_barrel_idx1, Last_barrel_idx, 
                Barrels).

    % The final whole list of barrels is the list [B|Barrels]
            

/*
    barrel(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, Barrel_idx, B) 
        B is the barrel of the grid with index First_barrel_idx

    B is a grid, represented as a list of lists (i.e. a matrix). 

    Arguments
    --------------
        - Grid_rows: list of list of int (matrix)
          Grid of cells.
        - N_columns_barrels: int
          Number of columns in the grid of barrels.
        - N_rows_per_barrel: int
          Number of rows in each barrel.
        - N_columns_per_barrel: int
          Number of columns in each barrel.
        - Barrel_idx: int
          Index of the barrel of interest.
        - B: list of list of int (matrix)
          Grid of the barrel.

*/            
barrel(Grid_rows, N_columns_barrels, N_rows_per_barrel, N_columns_per_barrel, Barrel_idx, B) :-
    Row_barrels #= (Barrel_idx div N_columns_barrels),  % Row index of that barrel
    Col_barrels #= (Barrel_idx mod N_columns_barrels),  % Column index of that barrel
  
    Starting_row_grid #= Row_barrels*N_rows_per_barrel+1,  % Starting row of that barrel in the grid of cells
    Ending_row_grid #= Starting_row_grid+N_rows_per_barrel-1,  % Ending row of that barrel in the grid of cells 
    Starting_col_grid #= Col_barrels*N_columns_per_barrel+1,  % Starting column of that barrel in the grid of cells
    Ending_col_grid #= Starting_col_grid+N_columns_per_barrel-1,  % Ending column of that barrel in the grid of cells
            
    select_submatrix(Starting_row_grid, Ending_row_grid, Starting_col_grid, Ending_col_grid, Grid_rows, B).

       
    


    