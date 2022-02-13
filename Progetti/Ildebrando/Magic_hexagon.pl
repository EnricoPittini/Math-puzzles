
:- use_module(library(clpfd)).	% Finite domain constraints

/* 
 * Non optimized version
 * magic_hexagon(Hex):-
		Hex = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
    	Hex ins 1..19,
    
        all_different(Hex),

        A + B + C #=  38,
        D + E + F + G #=  38,
        H + I + J + K + L #=  38, 
        M + N + O + P #=  38, 
        Q + R + S #=  38, 
        A + D + H #=  38, 
        B + E + I + M #=  38, 
        C + F + J + N + Q #=  38, 
        G + K + O + R #=  38, 
        L + P + S #=  38, 
        C + G + L #=  38, 
        B + F + K + P #=  38, 
        A + E + J + O + S #=  38, 
        D + I + N + R #=  38, 
        H + M + Q #=  38.
 * 
 */

%predicate that check if the sum of the elements of Hex is equal to the magic number
sum_magic(Hex) :- sum(Hex, #=, 38).


magic_hexagon(Hex) :-
    
%Elements of the hexagon
        Hex = [A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S],
    
%Elements of the hexagon in the integer range 1..19
        Hex ins 1..19,
    
%checks that all the elements of the Hexagon are different 
        all_different(Hex),
    
%maplist is a predicate that is true if it correctly applies a goal, in this case sum_magic, 
%on all matching element of the successive list, in this case a list of lists
    
        maplist(sum_magic, [[A,B,C], [D,E,F,G], [H,I,J,K,L], [M,N,O,P], [Q,R,S],
                        [H,D,A], [M,I,E,B], [Q,N,J,F,C], [R,O,K,G], [S,P,L],
                        [C,G,L], [B,F,K,P], [A,E,J,O,S], [D,I,N,R], [H,M,Q]]).


%This query allows to find the elements of the hexagon Hex and label them assigning a 
%value to each variable in Hex systematically trying out values for the finite domain 
%variables Hex until all of them are ground. The domain of each variable must be finite.
%The default option leftmost is selected such that labels variables in the order they 
%occur in Hex.
%
%?-magic_hexagon(Hex), labeling([leftmost], Hex).

