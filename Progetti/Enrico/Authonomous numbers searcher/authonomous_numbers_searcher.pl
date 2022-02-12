:- use_module(library(clpfd)).

/* 
  AUTHONOMOUS NUMBERS SEARCHER

  An authonomous number is a natural number N in which the digit 0 is not present and wich satisfies the following property: 
  counting the number of times each digit is used, from the smallest digit to the biggest digit, the number N, from left to 
  right, is itself found.
  
  Example
  21322314 
  The following digits are used:
    - Two 1 ;
    - Three 2 ;
    - Two 3 ;
    - One 4 ;
  Appending these results, the number 21322314 is found, left to right.
  
  
  
  AIM
  ---------------
  The aim is to find an authonomous number.
  
  This search is filtered with additional optional constraints, involving the number of digits of the number and the 
  divisibility of the number.
  
  For example, the user can specify that the authonomous number N must have a number of digits between 8 and 17 and that the 
  authonomous number must be divisible by 11 and 7 but not by 2 and 9.
  (In other words, n must have 11 and 7 as divisors but not 2 and 9).
  
   The user can ask about the divisibility of the authonomous number N by whatever number he wants. 
  
  
  
  EXAMPLE OF QUERY
  ---------------
  > authonomous_number(N, 1, 19, [-2,11-9]), label([N]).
  TRUE  N=3122331619
  N is an authonomous number, with number of digits between 1 and 19, which is not divisible by 2, which is divisible by 11
  and which is not divisible by 9.

  REFERENCES
  ---------------
  https://giochimatematici.unibocconi.it/index.php/archivio-giochi/archivio/2019/campionati-internazionali/finale-internazionale/finale-internazionale-2019-testi-seconda-giornata
*/



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% AUTHONOMOUS NUMBER

/*
    authonomous_number(N, MinL, MaxL, Ds) : N is an authonomous number.
    Additional recquirements:
    1. N has the number of digits between MinL and MaxL (extremes included);
    2. Ds is a list containing the recquired divisors for N.

    Arguments
    --------------
        - N: int
          Authonomous number.
        - MinL: int
          Minimum number of digits of the authonomous number.
        - MaxL: int
          Minimum number of digits of the authonomous number.
        - Ds: list of int
          List containing the recquired divisors for the number.
            * A positive number p means that the authonomous number N must have p as divisor.
            * A negative number -p means that the authonomous number N must not have p as divisor.

    Example
    --------------
    N=3122331619 MinL=1 MaxL=19 Ds=[-2,11,-9]
    TRUE
    N is an authonomous number, with number of digits between 1 and 19, which is not divisible by 2, which is divisible by 11
    and which is not divisible by 9.
*/
authonomous_number(N, MinL, MaxL, Ds) :-
    Min is 10**(MinL-1), % Minimum possible number in the domain of N
    Max is 10**MaxL-1, % Maximum possible number in the domain of N
    N in Min..Max, % Domain of N
    
    % L is the list containing the digits of N
    number_to_list(N, L),
    
    % Each element of N is a digit between 1 and 9
    L ins 1..9,
    
    % The number of digits is an even number
    length(L, N1), N1 mod 2 #= 0,  
    
    % The digits in an even position are in an ascending order
    evenPsxDigits_ascending_order(L),
    
    % The digits in an odd position count the next digits on the right
    oddPsxDigits_count_nextDigits(L),
    
    % All the digits in L which are in an odd position are contained in at least one digit in an even position
    all_oddPsxDigits_in_evenPsxDigits(L),
    
    % The list of numbers Ds contains the recquired divisors for N
    divisors(N, Ds).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% NUMBER TO LIST

/*
    append(L1, L2, L): L is the concatenation of L1 and L2 (i.e. L=L1.L2)

    Arguments
    --------------
        - L1: list
        - L2: list 
        - L: list
          L is the concatenation of L1 and L2
*/
append([],L2,L2).
append([H1|T1],L2,[H1|L]) :- 
    append(T1,L2,L).  % L is the concatenation between the tail T1 of the first list and the
					  % whole second list L2.
    % The result of the concatenation of the whole two lists is [H1|L], where H1 is the head of
    % the first list.


/*
    number_to_list(N, L) : L is the list containing the digits of N.

    Arguments
    --------------
        - N: int
          Positive number.
        - L: list of 0..9
          List containing the digits of N.

    Example
    --------------
    N=37812
    L=[3,7,8,1,2]
    TRUE
*/
% Base case: the number is only a single digit
number_to_list(N, [N]) :-
    N//10 #= 0.
% Recursuve case: the number consists in more digits
number_to_list(N, L) :-
    Q #= N//10,  % Number without the last digit (right-most)
    M #= N mod 10,  % Last digit (right-most)
    Q #> 0,  % N consists in more than one digit
    number_to_list(Q, L1),  % Recursive call: L1 is the list containing the digits of Q
    append(L1, [M], L).  % The final list L is obtained by appending L1 and [M]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ASCENDING ORDER EVEN-PSX-DIGITS

/*
    evenPsxDigits_ascending_order(L) : the digits in L which are in an even position are in a strictly ascending order.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 0..9
          List containing the digits of a certain number. The length is an even number.
          The digits in L which are in an even position are in a strictly ascending order.

    Example
    --------------
    L = [7, 6, 3, 8, 4, 9]
         1  2  3  4  5  6
    TRUE
    The digits in an even poisition are in a strictly ascending order.

    L = [7, 6, 3, 6, 4, 9]
         1  2  3  4  5  6
    FALSE
    The digits in an even poisition are not in a strictly ascending order.
*/
evenPsxDigits_ascending_order(L) :-
    % Call evenPsxDigits_ascending_order_AUX on L, with additional argument X equal to -1
    evenPsxDigits_ascending_order_AUX(L, -1).

/*
    evenPsxDigits_ascending_order_AUX(L, X) : the first digit in L which is in an even position is strictly bigger than X AND
    the digits in L which are in an even position are in a strictly ascending order.

    Basically, this predicate represents the imperative implementation of the evenPsxDigits_ascending_order predicate.
    It has an additional argument (i.e. X): the first digit in an even position must be strictly bigger than X.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 1..9
          List containing the digits of a certain number. The length is an even number.
        - X: 1..9
    The first digit in L which is in an even position is strictly bigger than X AND the digits in L which are in an even 
    position are in a strictly ascending order.

    Example
    --------------
    L = [7, 6, 3, 8, 4, 9]      X = 3
         1  2  3  4  5  6
    TRUE
    The first digit which is in an even position is 6, which is bigger than X. 
    All the other digits which are in an even position are in an ascending order.

    L = [7, 6, 3, 8, 4, 9]      X = 9
         1  2  3  4  5  6
    FALSE
    The first digit which is in an even position is 6, which is not bigger than X. 
*/
% Base case: empty list. It is always True
evenPsxDigits_ascending_order_AUX([], _).
% Recursive case: the list is not empty. 
% Since the list has an even length, it has at least two elements: L=[H1,H2|T].
evenPsxDigits_ascending_order_AUX([_,H2|T], X) :-
    % H2 is the second element of the list: it is the first digit in an even position.
    % H2 must be bigger than X.
    H2 #> X,
    % Recursive call: the first digit in the tail T which is in an even position must be greater than H2 AND the digits in T 
    % which are in an even position must be in an ascending order
    evenPsxDigits_ascending_order_AUX(T, H2).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ODD-PSX-DIGITS COUNT NEXT DIGITS

/*
    number_of_occourances(L, X, N) : in L there are N elements equal to X.

    Arguments
    --------------
        - L: list of int 
        - X: int
        - N: int

    Example
    --------------
    L = [7, 6, 4, 8, 4, 9]              X = 4                        N = 2
    TRUE
    In L there are 2 elements equal to 4

*/
% Base case: empty list
number_of_occourances([], _, 0).
% Recursive case: the list has at least one element and the head is equal to X
number_of_occourances([H|T], X, N) :-
% TODO: sistemare confronto di uguaglianza
    H #= X,  % The head is equal to X
    number_of_occourances(T, X, N1),  % N1 is the number of elements in the tail T which are equal to X
    N #= N1+1.  % N is the result: it is equal to N1+1
% Recursive case: the list has at least one element and the head is not equal to X
number_of_occourances([H|T], X, N) :-
    H #\= X,  % The head is not equal to X
    number_of_occourances(T, X, N).  % N is the number of elements in the tail T which are equal to X
                                     % N is the result


/*
    oddPsxDigits_count_nextDigits(L) : the digits in L which are in an odd position count the number of occourances of the
    digits next to them on the right.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 0..9
          List containing the digits of a certain number. The length is an even number.
          The digits in L which are in an odd position count the number of occourances of the digits next to them on the 
          right.

    Example
    --------------
    L = [4, 4, 4, 4]
         1  2  3  4
    TRUE
        - The digit 4 in position 1 is the number of times the next digit on the right (i.e. digit 4 in position 2) is 
          present in L.
        - The digit 4 in position 3 is the number of times the next digit on the right (i.e. digit 4 in position 4) is 
          present in L.

    L = [3, 2, 2, 1, 1, 4]
         1  2  3  4  5  6 
    TRUE
        - The digit 3 in position 1 is the number of times the next digit on the right (i.e. digit 2 in position 2) is 
          present in L.
        - The digit 2 in position 3 is the number of times the next digit on the right (i.e. digit 1 in position 4) is 
          present in L.
        - The digit 1 in position 5 is the number of times the next digit on the right (i.e. digit 4 in position 6) is 
          present in L.

*/
oddPsxDigits_count_nextDigits(L) :- 
    % Call oddPsxDigits_count_nextDigits_AUX on L, with additional argument L1 equal to L
    oddPsxDigits_count_nextDigits_AUX(L, L).

/*
    oddPsxDigits_count_nextDigits_AUX(L, L1) : the digits in L which are in an odd position count the number of occourances 
    in L1 of the next digit to the right in L.  

    Basically, this predicate represents the implementation of the oddPsxDigits_count_nextDigits_AUX predicate.
    It has an additional argument (i.e. L1): the count of the digits in L is done in L1.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 1..9
          List containing the digits of a certain number. The length is an even number.
        - L1: list of 1..9
    The digits in L which are in an odd position count the number of occourances in L1 of the next digit to the right in L. 

    Example
    --------------
    L = [2, 6, 3, 8]      L1 = [6, 8, 8, 6, 8]
         1  2  3  4  
    TRUE
        - The digit 2 in position 1 is the number of times the next digit on the right (i.e. digit 6 in position 2) is 
          present in L1.
        - The digit 3 in position 3 is the number of times the next digit on the right (i.e. digit 8 in position 4) is 
          present in L1.

*/
% Base case: L is an empty list
oddPsxDigits_count_nextDigits_AUX([], _).
% Recursive case: L is not empty.
% Since it has an even length, it has at least two elements: L=[H1,H2|T].
oddPsxDigits_count_nextDigits_AUX([H1,H2|T], L1) :-
    % H1 is the first digit in L which has an odd position; H2 is the next digit to the right.
    % H1 must be equal to the number of occourances of H2 in L1.
    number_of_occourances(L1, H2, N),  % N is the number of occourances of H2 in L1
    H1 #= N,  % H1 is equal to N
    oddPsxDigits_count_nextDigits_AUX(T, L1).  % Recursive call on the tail T of L, with the same L1



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ALL ODD-PSX-DIGITS IN EVEN-PSX-DIGITS

/*
    evenPsxDigit(L, X) : the digit X is equal to a digit in an even position in L.

    Arguments
    --------------
        - L: list of 1..9
          It represents the list of digits of a certain number. It has a length which is an even number.
        - X: 1..9
          It is a digit.

    Example
    --------------
    L = [7, 4, 6, 4, 8, 9]             X = 4
         1  2  3  4  5  6
    TRUE

    L = [7, 6, 4, 8, 5, 9]             X = 4
         1  2  3  4  5  6
    FALSE   

*/
% Base case: the first even-position digit (i.e. H2) is equal to X.
evenPsxDigit([_,X | _], X).
% Recursive case: the first even-position digit (i.e. H2) is not equal to X.
evenPsxDigit([_,H2 | T], X) :-
    H2 #\= X,
    evenPsxDigit(T, X).  % Recursive call on the tail T of the list


/*
    all_oddPsxDigits_in_evenPsxDigits(L) : all the digits in L which are in an odd position are contained in at least one 
    digit in L which is an even position.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 0..9
          List containing the digits of a certain number. The length is an even number.
          All the digits in L which are in an odd position are contained in at least one digit in L which is an even 
          position.

    Example
    --------------
    L = [4, 3, 1, 4, 4, 1]
         1  2  3  4  5  6
    TRUE
    All the digits in an odd position (i.e. 4, 1 and 4) are contained in at least one digit in an even position.

    L = [1, 2, 1, 3]
         1  2  3  4  
    FALSE
    The digit 1 (which is in an odd position) is not contained in any digit in an even position.

*/
all_oddPsxDigits_in_evenPsxDigits(L) :-
    % Call all_oddPsxDigits_in_evenPsxDigits_AUX on L1, with additional argument L1 equal to L
    all_oddPsxDigits_in_evenPsxDigits_AUX(L, L).

/*
    all_oddPsxDigits_in_evenPsxDigits_AUX(L, L1) : all the digits in L which are in an odd position are contained in at least
    one digit in L1 which is an even position.

    Basically, this predicate represents the implementation of the all_oddPsxDigits_in_evenPsxDigits_AUX predicate.
    It has an additional argument (i.e. L1): the search of the odd-position digits in L is done in L1.

    The position of the digits is built left to right, starting from 1.

    Arguments
    --------------
        - L: list of 1..9
          List containing the digits of a certain number. The length is an even number.
        - L1: list of 1..9
          The length is an even number.
    All the digits in L which are in an odd position are contained in at least one digit in L1 which is an even position. 

    Example
    --------------
    L = [2, 6, 3, 8]      L1 = [6, 2, 2, 2, 3, 2]
         1  2  3  4             1  2  3  4  5  6
    TRUE
    All the digits in an odd position (i.e. 2 and 3) are contained in at least one digit in an even position in L1.

*/
% Base case: the list L is exmpty
all_oddPsxDigits_in_evenPsxDigits_AUX([], _).
% Recursive case: the list L is not empty.
% Since the length of L is an even number, L contains at least two elements: L=[H1,H2|T].
all_oddPsxDigits_in_evenPsxDigits_AUX([H1,_|T], L1) :-
    evenPsxDigit(L1, H1),  % H1 is contained in a digit in an even position in L1
    all_oddPsxDigits_in_evenPsxDigits_AUX(T, L1).  % Recursive call on the tail T of L



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DIVISORS

/*
    divisors(N, Ds) : the list Ds specifies the recquired divisors for N.

    Arguments
    --------------
        - N: int
        - Ds: list of int
          List containing the recquired divisors for the number.
            * A positive number p means that the authonomous number N must have p as divisor.
            * A negative number -p means that the authonomous number N must not have p as divisor.

*/
% Base case: empty list
divisors(_, []).
% Recursive case: the list Ds has at least one element, which is positive
divisors(N, [H|T]) :-
    H #>0,  % The head of the list is positive
    N mod H #= 0,  % The number N must be divisible by H
    divisors(N, T).  % Recursive call on the tail T of the list
% Recursive case: the list Ds has at least one element, which is negative
divisors(N, [H|T]) :-
    H #<0,  % The head of the list is negative
    N mod abs(H) #> 0,  % The number N must be not divisible by H
    divisors(N, T).  % Recursive call on the tail T of the list