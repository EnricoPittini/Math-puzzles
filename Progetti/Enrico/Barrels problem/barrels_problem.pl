:- use_module(library(clpfd)).
:- use_rendering(table).

barrels_problem(Grid_rows, Numbers_barrelsTop, N_rows_barrels, N_columns_barrels,
                N_rows_per_barrel, N_columns_per_barrel, Max_number, 
                Recquired_numbers_barrelsTop) :-
    Total_rows #= N_rows_barrels*N_rows_per_barrel,
    Total_columns #= N_columns_barrels*N_columns_per_barrel,    
    N_barrels #= N_rows_barrels*N_columns_barrels,
    
    length(Grid_rows, Total_rows), maplist(len(Total_columns), Grid_rows),
    append(Grid_rows, Grid), Grid ins 1..Max_number,
    maplist(all_distinct, Grid_rows),
    transpose(Grid_rows, Grid_columns),
    maplist(all_distinct, Grid_columns),
    
    Last_barrel_idx #= N_barrels-1,
    barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
            N_columns_per_barrel, Barrels, 0, Last_barrel_idx),

    maplist(append, Barrels, Barrels_flat), 
	maplist(all_distinct, Barrels_flat),
    
    maplist(sum, Barrels_flat, Numbers_barrelsTop),
    
    all_distinct(Numbers_barrelsTop),
    
    members(Numbers_barrelsTop, Recquired_numbers_barrelsTop).
    
% Base case: the list L is empty L=[]
sum([],0).
% Recursive case: the list L has at least one element L=[H|T]
sum([H|T],S1) :- 
    sum(T,S),  % Recursive call: S is the sum of the elements in the tail T
    S1 #= H+S.  % S1 is equal to the sum H+S

members(_, []).
members(L, [H|T]) :-
    memberchk(H, L),
    members(L, T).
    

len(N,L) :- length(L,N).

label_listOfLists([]).
label_listOfLists([H|T]) :-
    label(H),
    label_listOfLists(T).

% TODO delete
label_listOfListsOfLists([]).
label_listOfListsOfLists([H|T]) :-
    label_listOfLists(H),
    label_listOfListsOfLists(T).

barrels(_, _, _, _, _, [], Barrel_idx, Last_barrel_idx):- Barrel_idx #> Last_barrel_idx.
barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
        N_columns_per_barrel, [B|Barrels], Barrel_idx, Last_barrel_idx) :-
    barrel(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
           N_columns_per_barrel, Barrel_idx, B),
    Barrel_idx1 #= Barrel_idx+1,
    barrels(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
            N_columns_per_barrel, Barrels, Barrel_idx1, Last_barrel_idx).
            
barrel(Grid_rows, N_rows_barrels, N_columns_barrels, N_rows_per_barrel, 
       N_columns_per_barrel, Barrel_idx, B) :-
    Row_barrels #= (Barrel_idx div N_columns_barrels),
    Col_barrels #= (Barrel_idx mod N_columns_barrels),
  
    Starting_row_grid #= Row_barrels*N_rows_per_barrel+1,
    Ending_row_grid #= Starting_row_grid+N_rows_per_barrel-1,
    Starting_col_grid #= Col_barrels*N_columns_per_barrel+1,
    Ending_col_grid #= Starting_col_grid+N_columns_per_barrel-1,
            
    select_submatrix(Starting_row_grid, Ending_row_grid, Starting_col_grid, 
                     Ending_col_grid, Grid_rows,  B).


select_sublist(Start, End, [_|T], L1) :- 
    Start #> 1,
    Start #=< End,
    Start1 #= Start-1, End1 #= End-1,
    select_sublist(Start1, End1, T, L1).
select_sublist(Start, End, _, []) :- 
    Start #> End.
select_sublist(Start, End, [H|T], [H|L1]) :- 
    Start #= 1,
    Start #=< End,
    End1 #= End-1,
    select_sublist(Start, End1, T, L1).

select_submatrix(Starting_row, Ending_row, Starting_col, Ending_col, M, M1) :-
    select_sublist(Starting_row, Ending_row, M, M_temp),
    maplist(select_sublist(Starting_col, Ending_col), M_temp, M1).
       
    


    