:- use_module(library(clpfd)).

trains([[1,2,0,1],
        [2,3,4,5],
        [2,3,0,1],
        [3,4,5,6],
        [3,4,2,3],
        [3,4,8,9]]).

		
path(A, A, []).

path(A, B, Ps) :-
        Ps = [[A,B,T0,T1]],
        trains(Ts),
        tuples_in(Ps, Ts).	

path(A, Z, Ps) :-
        Ps = [[A,B,T0,T1],[B,Z,T2,T3]],
        T2 #> T1, 
        trains(Ts),
        tuples_in(Ps, Ts).		

path(A, Z, Ps) :-
        Ps = [[A,B,T0,T1],[B,C,T2,T3]|T],
		path(B, Z, [[B,C,T2,T3]|T]),
        T2 #> T1, 
        trains(Ts),
        tuples_in(Ps, Ts).
		

