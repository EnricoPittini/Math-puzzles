:- use_module(library(clpfd)).

authonomous_number(N, MinL, MaxL, Ds) :-
    %U is 10**18,
    Min is 10**(MinL-1),
    Max is 10**(MaxL-1),
    N in Min..Max,
    
    divisors(N, Ds),
    
    number_to_list(N, L),
    
    even_length(L),
    
    %non_zero_digits(L),
    L ins 1..9,
    
    ascending_order(L),
    
    digits_count(L),
    
    all_digits(L).

divisors(_, []).
divisors(N, [H|T]) :-
    H #>0,
    N mod H #= 0,
    divisors(N, T).
divisors(N, [H|T]) :-
    H #<0,
    N mod abs(H) #> 0,
    divisors(N, T).

number_to_list(N, [N]) :-
    N//10 #= 0.
/*number_to_list(N, L) :-
    N//10 #> 0,
    number_to_list(N//10, L1),
    append(L1, [N mod 10], L), !.*/
number_to_list(N, L) :-
    Q #= N//10,
    M #= N mod 10,
    Q #> 0,
    number_to_list(Q, L1),
    append(L1, [M], L).

% append(L1, L2, L): L is the concatenation of L1 and L2 (i.e. L=L1.L2)
append([],L2,L2).
append([H1|T1],L2,[H1|L]) :- 
    append(T1,L2,L). % L is the concatenation between the tail T1 of the first list and the
					 % whole second list L2.
    % The result of the concatenation of the whole two lists is [H1|L], where H1 is the head of
    % the first list.

even_length([]).
even_length([_,_|T]) :-
    even_length(T).
    

non_zero_digits([]).
non_zero_digits([H|T]) :-
    H #\= 0,
    non_zero_digits(T).

ascending_order_aux([], _).
ascending_order_aux([_,X|T], P) :-
    X #> P,
    ascending_order_aux(T, X).
ascending_order(L) :-
    ascending_order_aux(L, -1).

digits_count(L) :- 
    digits_count_aux(L, L).
digits_count_aux([], _).
digits_count_aux([H1,H2|T], L) :-
    number_of_occourances(H2, L, N),
    H1 #= N,
    digits_count_aux(T, L).

number_of_occourances(_, [], 0).
number_of_occourances(X, [H|T], N) :-
    H #= X,
    number_of_occourances(X, T, N1),
    N #= N1+1.
number_of_occourances(X, [H|T], N) :-
    H #\= X,
    number_of_occourances(X, T, N).

all_digits(L) :-
    all_digits_aux(L, L).
all_digits_aux([], _).
all_digits_aux([H,_|T], L) :-
    member_even(L, H),
    all_digits_aux(T, L).

member_even([_,H | _], H).
member_even([_,H | T], X) :-
    H #\= X,
    member_even(T, X).
    