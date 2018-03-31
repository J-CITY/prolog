%a^nb^n
%accept(+-)
initial(q0).
final(q3).

delta(q0, a, S, q1, [a|S]).
delta(q1, a, S, q1, [a|S]).
delta(q1, a, S, q2, [a|S]).
delta(q2, b, [H|T], q2, T).
delta(q2, b, [H|T], q3, T).

delta(q0, a, S, q4, [a|S]).
delta(q4, b, [H|T], q3, T).

accept(L) :- initial(Q), accept(L,Q, []).

accept([H|T], Q, S) :- delta(Q, H, S, Q1, S1), accept(T, Q1, S1). 

accept([], Q, []) :- final(Q).




test0() :- not(accept([a])).
test1() :- not(accept([b])).
test2() :- not(accept([a,a])).
test3() :- not(accept([b,b])).
test4() :- not(accept([z,c])).
test5() :- not(accept([b, b])).
test6() :- not(accept([a, b, b])).
test7() :- not(accept([a, a, b])).
test8() :- not(accept([])).

test9() :- accept([a,b]).
test10() :- accept([a,a,b,b]).
test11() :- accept([a,a,a,b,b,b]).

test() :- test0(),test1(),test2(),test3(),test4(),test5(),test6(),test7(),test8(),test9(), test10(), test11().
