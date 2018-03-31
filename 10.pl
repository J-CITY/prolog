%ab*c
%accept(+-)

initial(q0).
final(q3).

delta(q0, a, q1).
delta(q1, b, q2).
delta(q1, b, q1).
delta(q2, c, q3).

accept(L) :- initial(Q), accept(L,Q).

accept([H|T], Q) :- delta(Q, H, Q1), accept(T, Q1). 

accept([], Q) :- final(Q).


test0() :- not(accept([a])).
test1() :- not(accept([a,b])).
test2() :- not(accept([a,c])).
test3() :- not(accept([b,b])).
test4() :- not(accept([b])).
test9() :- not(accept([])).

test5() :- accept([a,b,c]).
test6() :- accept([a,b,b,c]).
test7() :- accept([a,b,b,b,c]).
test8() :- accept([a,b,b,b,b,b,b,b,b,b,b,b,b,c]).

test() :- test0(),test1(),test2(),test3(),test4(),test5(),test6(),test7(),test8(),test9().
