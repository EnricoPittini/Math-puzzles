
:- use_module(library(clpfd)).	% Finite domain constraints

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


%This query allows to find the elements of the hexagon Hex and label them assign a 
%value to each variable in Hex systematically trying out values for the finite domain 
%variables Hex until all of them are ground. The domain of each variable in must be finite.
%The default option leftmost is selected such that labels  the variables in the order they 
%occur in Hex.
%
%
%
%?-magic_hexagon(Hex), labeling([leftmost], Hex).