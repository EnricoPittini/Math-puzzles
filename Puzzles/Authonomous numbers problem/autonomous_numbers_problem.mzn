/* 
  AUTONOMOUS NUMBERS PROBLEM

  An autonomous number is a natural number N in which the digit 0 is not present and wich satisfies the following property:
  counting the number of times each digit is used, from the smallest digit to the biggest digit, the number N is itself 
  found, from left to right.
  
  Example
  21322316 
  The following digits are used:
    - Two 1 ;
    - Three 2 ;
    - Two 3 ;
    - One 6 ;
  Appending these results, the number 21322316 is found, left to right.
  
  
  
  AIM
  ---------------
  The aim is to find an autonomous number.
  
  This search is filtered with additional optional constraints, involving the number of digits of the number and the 
  divisibility of the number.
  
  For example, the user can specify that the autonomous number N must have a number of digits between 8 and 17 and that the 
  autonomous number must be divisible by 11 and 7 but not by 2 and 9.
  (In other words, N must have 11 and 7 as divisors but not 2 and 9).
  
  The user can ask about the divisibility of the autonomous number N by numbers in the range 2..11. 
  (The reason of the limitation 2..11 will be explained later on)
  
  
  
  PARAMETERS
  ---------------
    - int: min_length
      Minimum number of digits of the autonomous number.
    - int: max_length
      Maximum number of digits of the autonomous number.

    - int: n_divisors
      Number of recquired divisors for the autonomous number.
    - array[1..n_divisors] of -11..11: divisors
      Array containing the recquired divisors for the autonomous number.
      Each element of this array is a number in -11..11.
        * The numbers -10,-1,0-1,10 are not considered.
        * A positive number p means that the autonomous number N must have p as divisor.
        * A negative number -p means that the autonomous number N must not have p as divisor.
        
        
        
  IMPLEMENTATION
  ---------------
  The autonomous number is implemented not as a number, but as an array of digits.
  Basically, not a single integer variable, but an array of variables in 0..9.
  
  This is done because of the integer overflow.
  Indeed, in MinZinc, an operation between integer variables overflows if it exceeds 10^11: this is a big limitation for our 
  problem. (The biggest autonomous number has 18 digits).

  This is the reason why the user can't specify generic divisors, but only divisors in the range 2..11.
  Indeed, for the numbers in the range 2..11, there are divisibility rules which can be applied on the array of digits, 
  rather than on the actual number.



  REFERENCES
  ---------------
  https://giochimatematici.unibocconi.it/index.php/archivio-giochi/archivio/2019/campionati-internazionali/finale-internazionale/finale-internazionale-2019-testi-seconda-giornata
*/




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% PARAMETERS
% Minimum and maximum number of digits of the autonomous number
int: min_length;
int: max_length;

% Recquired divisors for the autonomous number
int: n_divisors;
array[1..n_divisors] of -11..11: divisors;
% The elements are in the range -11..11.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% VARIABLES
% Authonomous number, represented as an array of digits
array[1..max_length] of var 0..9: N;


/*
  Notes
  ------------
  Also the 0 digit is used in the autonomous number, even if, by definition, an autonomous number can't contain the 0 
  digit.
  This is done in order to be able to handle a variable length.
  
  Indeed, the length of N it is fixed, and it is `max_length`, while the actual length is `length` (i.e. the actual number of
  digits).
  All the digits from `length`+1 up to ` max_length` are useless digits, and they are all 0. 
  `length` is, in general, not known (it is known only inside the exists constraint): therefore, the 0 digit is used for 
  recognizing the useless digits.
*/




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USER-DEFINED PREDICATES
/*
*  autonomous_number: the given integer number, represented as an array of digits, is an autonomous number.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*/
predicate autonomous_number(array[1..max_length] of var 0..9: N, int: length) =
    % All the digits must be non-zero
    forall(i in 1..length)(N[i]>0) /\
    % The digits with even index must be in a strict ascending order
    forall(i in 1..length where i mod 2==0 /\ i>2)(N[i]>N[i-2]) /\
    % For each digit with even index, the previous digit in the number must represent the count of that digit in the number
    forall(i in 1..length where i mod 2==0)(sum([N[j]==N[i] | j in 1..length])==N[i-1]) /\
    % Each digit with an odd index must be equal to at least one digit with an even index
    forall(i in 1..length where i mod 2==1)(exists(j in 1..length where j mod 2==0)(N[j]==N[i]));
    
/*
*  divisible_by_2: the given integer number, represented as an array of digits, is divisible by 2.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 2 iif the last digit is divisible by .
*/
predicate divisible_by_2(array[1..max_length] of var 0..9: N, int: length) = N[length] mod 2 == 0;

/*
*  divisible_by_3: the given integer number, represented as an array of digits, is divisible by 3.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 3 iif the sum of the digits is divisible by 3.
*
*/
predicate divisible_by_3(array[1..max_length] of var 0..9: N, int: length) = sum(i in 1..length)(N[i]) mod 3 == 0;

/*
*  divisible_by_4: the given integer number, represented as an array of digits, is divisible by 4.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 4 iif the last two digits are divisible by 4.
*
*/
predicate divisible_by_4(array[1..max_length] of var 0..9: N, int: length) = 
    (length>=2 /\ (N[length-1]*10+N[length]) mod 4 == 0) \/ (length<2 /\ N[length] mod 4 == 0);

/*
*  divisible_by_5: the given integer number, represented as an array of digits, is divisible by 5.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 5 iif the last digit is divisible by 5.
*
*/
predicate divisible_by_5(array[1..max_length] of var 0..9: N, int: length) = N[length] mod 5 == 0;

/*
*  divisible_by_6: the given integer number, represented as an array of digits, is divisible by 6.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 6 iif it is divisible by 2 and 3.
*
*/
predicate divisible_by_6(array[1..max_length] of var 0..9: N, int: length) = 
    divisible_by_2(N, length) /\ divisible_by_3(N, length);

/*
*  divisible_by_7: the given integer number, represented as an array of digits, is divisible by 7.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*  It is assumed that N has at least three digits (i.e. length>=3).
*
*  Implementation
*  ------------
*  A number is divisible by 7 iif the sum of the following three quantities is divisible by 7.
*      1. The alternating sum of the digits whose index is equal to 0 mod 3.
*      2. Three times the alternating sum of the digits whose index is equal to 1 mod 3.
*      3. Two times the alternating sum of the digits whose index is equal to 2 mod 3.
*  N.B. the count of the position of the digits starts from 0, right to left.
*  
*  Example
*  777213213
*  (3-3+7) + 3*(1-1+7) + 2*(2-2+7) = 42, which is divisible by 7.
*
*/
predicate divisible_by_7(array[1..max_length] of var 0..9: N, int: length) = let {
  %%%% Local variables
  
  % Digits whose index is equal to 0 mod 3
  1..length: length_index_zero = if (length mod 3 == 0) then (length div 3) else ((length div 3) + 1) endif;
  array[1..length_index_zero] of var 1..9: digits_index_zero = [N[length-j] | j in 0..length-1 where j mod 3 == 0];
  
  % Digits whose index is equal to 1 mod 3
  1..length: length_index_one = if (length mod 3 != 2) then (length div 3) else ((length div 3) + 1) endif;
  array[1..length_index_one] of var 1..9: digits_index_one = [N[length-j] | j in 0..length-1 where j mod 3 == 1];
  
  % Digits whose index is equal to 2 mod 3
  1..length: length_index_two = length div 3;
  array[1..length_index_two] of var 1..9: digits_index_two = [N[length-j] | j in 0..length-1 where j mod 3 == 2];
  
} in 
    % Alternating sum of the digits whose index is equal to 0 mod 3
    (sum(i in 1..length_index_zero)(if i mod 2 == 1 then digits_index_zero[i] else -digits_index_zero[i] endif) +
    % Three times the alternating sum of the digits whose index is equal to 1 mod 3
    sum(i in 1..length_index_one)(if i mod 2 == 1 then digits_index_one[i] else -digits_index_one[i] endif)*3 + 
    % Two times the alternating sum of the digits whose index is equal to 2 mod 3
    sum(i in 1..length_index_two)(if i mod 2 == 1 then digits_index_two[i] else -digits_index_two[i] endif)*2) mod 7 == 0; 
  
/*
*  divisible_by_8: the given integer number, represented as an array of digits, is divisible by 8.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 8 iif the last three digits are divisible by 8.
*
*/  
predicate divisible_by_8(array[1..max_length] of var 0..9: N, int: length) = 
    (length>=3 /\ ((N[length-2]*100+N[length-1]*10+N[length]) mod 8 == 0)) \/ 
    (length==2 /\ ((N[length-1]*10+N[length]) mod 8 == 0)) \/
    (length<2 /\ (N[length] mod 8 == 0));
    %(N[length-2]*100+N[length-1]*10+N[length]) mod 8 == 0;
   
/*
*  divisible_by_9: the given integer number, represented as an array of digits, is divisible by 9.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 9 iif the sum of the digits is divisible by 9.
*
*/ 
predicate divisible_by_9(array[1..max_length] of var 0..9: N, int: length) = sum(i in 1..length)(N[i]) mod 9 == 0;

/*
*  divisible_by_11: the given integer number, represented as an array of digits, is divisible by 11.
*  
*  Input
*  ------------
*      - array[1..max_length] of var 0..9: N
*        Integer number, represented as an array of digits.
*      - int: length
*        Actual number of digits of the number (i.e. actual length of the array).
*
*  Implementation
*  ------------
*  A number is divisible by 11 iif the absolute difference between the digits in odd position and the digits in even position
*  is divisible by 11.
*
*/
predicate divisible_by_11(array[1..max_length] of var 0..9: N, int: length) = 
    abs(sum(i in 1..length where i mod 2 == 1)(N[i]) - sum(i in 1..length where i mod 2 == 0)(N[i])) mod 11 == 0;




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% CONSTRAINTS
% Search if there exist a number of digits (i.e. `length`) such that there is a number with that length which satisfies 
% all the contraints 
constraint exists(length in min_length..max_length where length mod 2 == 0)(
  % `length` represents the actual number of digits.
  % `length` is accessible only inside this exists constraint.
  
  % The number is autonomous
  autonomous_number(N, length) /\
  
  % Divisibility by 2
  (has_element(2, divisors) -> divisible_by_2(N, length)) /\
  (has_element(-2, divisors) -> not divisible_by_2(N, length)) /\
  
  % Divisibility by 3
  (has_element(3, divisors) -> divisible_by_3(N, length)) /\
  (has_element(-3, divisors) -> not divisible_by_3(N, length)) /\
  
  % Divisibility by 4
  (has_element(4, divisors) -> divisible_by_4(N, length)) /\
  (has_element(-4, divisors) -> not divisible_by_4(N, length)) /\
  
  % Divisibility by 5
  (has_element(5, divisors) -> divisible_by_5(N, length)) /\
  (has_element(-5, divisors) -> not divisible_by_5(N, length)) /\
  
  % Divisibility by 6
  (has_element(6, divisors) -> divisible_by_6(N, length)) /\
  (has_element(-6, divisors) -> not divisible_by_6(N, length)) /\
  
  % Divisibility by 7
  (has_element(7, divisors) -> (length>2 /\ divisible_by_7(N, length))) /\
  (has_element(-7, divisors) -> (length<=2 \/ not divisible_by_7(N, length))) /\
  
  % Divisibility by 8
  (has_element(8, divisors) -> divisible_by_8(N, length)) /\
  (has_element(-8, divisors) -> not divisible_by_8(N, length)) /\
  
  % Divisibility by 9
  (has_element(9, divisors) -> divisible_by_9(N, length)) /\
  (has_element(-9, divisors) -> not divisible_by_9(N, length)) /\
  
  % Divisibility by 11
  (has_element(11, divisors) -> divisible_by_11(N, length)) /\
  (has_element(-11, divisors) -> not divisible_by_11(N, length)) 
);

solve satisfy;




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% OUTPUT

% In showing the number, the 0 digits are removed, because they are the useless digits from `length`+1 up to `max_length`
output [show([N[i] | i in 1..max_length where N[i]>0])] ;
