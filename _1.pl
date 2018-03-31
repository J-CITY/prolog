sent(sent(NP,VP)) --> np(NP, Num), vp(VP, Num).

np(np(Det,N), Num) --> det(Det, Num), n(N, Num).

np(np(N), Num) --> n(N, Num).

np(np(Adj, N), Num) --> adj(Adj, Num), n(NP, Num).
np(np(Det,N,PP), Num) --> det(Det, Num), n(N, Num), pp(PP).

vp(vp(V), Num) --> v(V, Num).
vp(vp(V,NP), Num) --> v(V, Num), np(NP, _).
vp(vp(V,NP,PP), Num) --> v(V, Num), np(NP, _), pp(PP).

pp(pp(P,NP)) --> p(P), np(NP, _).


det(det(a), sg) --> [a].
det(det(the), _) --> [the].

n(n(man), sg) --> [man].
n(n(men), pl) --> [men].
n(n(coffee), sg) --> [coffee].
n(n(sugar), sg) --> [sugar].
n(n(plane), sg) --> [plane].
n(n(planes), pl) --> [planes].

n(n(he), sq) --> [he].
n(n(they), pl) --> [they].

v(v(like), pl) --> [like].
v(v(likes), sq) --> [likes].
v(v(saw), _) --> [saw].

p(p(with)) --> [with].

adj(adj(hot), _) --> [hot].

%phrase(sent(Ans), [he, likes, coffee, with, a, sugar]).
test0() :- phrase(sent(Ans), [he, likes, coffee, with, a, sugar]),
	Ans = sent(np(n(he)), vp(v(likes), np(n(coffee)), pp(p(with), np(det(a), n(sugar))))).
test1() :- phrase(sent(sent(np(n(he)), vp(v(likes), np(n(coffee)), pp(p(with), np(det(a), n(sugar)))))), 
	[he, likes, coffee, with, a, sugar]).
%phrase(sent(Ans), [he, likes, hot, coffee, with, a, sugar]).
test2() :- phrase(sent(Ans), [he, likes, hot, coffee, with, a, sugar]), 
	Ans = sent(np(n(he)), vp(v(likes), np(adj(hot), _2632), pp(p(with), np(det(a), n(sugar))))).
%phrase(sent(Ans), [they, like, coffee, with, a, sugar]).
test3() :- phrase(sent(Ans), [they, like, coffee, with, a, sugar]), 
	Ans = sent(np(n(they)), vp(v(like), np(n(coffee)), pp(p(with), np(det(a), n(sugar))))).
%phrase(sent(Ans), [he, saw, a, plane]).
test4() :- phrase(sent(Ans), [he, saw, a, plane]),
	Ans = sent(np(n(he)), vp(v(saw), np(det(a), n(plane)))).
%phrase(sent(Ans), [he, saw, the, planes]).
test5() :- phrase(sent(Ans), [he, saw, the, planes]), 
	Ans = sent(np(n(he)), vp(v(saw), np(det(the), n(planes)))).
%phrase(sent(Ans), [they, saw, the, planes]).
test6() :- phrase(sent(Ans), [they, saw, the, planes]), 
	Ans = sent(np(n(they)), vp(v(saw), np(det(the), n(planes)))).
test7() :- not(phrase(sent(Ans), [he, like, hot, coffee, with, a, sugar])).
test8() :- phrase(sent(Ans), L), Ans = sent(np(det(a), n(man)), vp(v(saw))), L = [a, man, saw].
test9() :- phrase(sent(Ans), L), Ans = sent(np(det(a), n(man)), vp(v(saw), np(det(the), n(man)))),
	L = [a, man, saw, the, man].
test10():- phrase(sent(Ans), L), Ans = sent(np(det(a), n(man)), vp(v(saw), np(det(a), n(plane)))),
	L = [a, man, saw, a, plane].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

num(0) --> [zero].
num(N) --> n4(N).

n4(N) --> digit(D), [thousand], {N is D*1000}.%%%
n4(N) --> digit(D), [thousand], n3(N1), {N is D*1000+N1}.
n4(N) --> n3(N).
n4(N) --> n21(N).

n3(N) --> digit(D), [hundred], {N is D*100}.%%%
n3(N) --> digit(D), [hundred], rest_n4(N1), {N is D*100+N1}.
n3(N) --> n21(N).

rest_n4(0) --> [zero].
rest_n4(N) --> n21(N).

n21(N) --> digit(N). 
n21(N) --> teen(N). 
n21(N) --> tens(N). 
n21(N) --> rest_t(T) ,rest21(N1), {N is T + N1}.

rest21(0) --> [zero].
rest21(N) --> digit(N).

rest_t(0) --> [zero].
rest_t(N) --> tens(N).

digit(1) --> [one].
digit(2) --> [two].
digit(3) --> [three].
digit(4) --> [four].
digit(5) --> [five].
digit(6) --> [six].
digit(7) --> [seven].
digit(8) --> [eight].
digit(9) --> [nine].

teen(11) --> [eleven].
teen(12) --> [twelve].
teen(13) --> [thirteen].
teen(14) --> [fourteen].
teen(15) --> [fifteen].
teen(16) --> [sixteen].
teen(17) --> [seventeen].
teen(18) --> [eighteen].
teen(19) --> [nineteen].

tens(10) -->[ten].
tens(20) -->[twenty].
tens(30) -->[thirty].
tens(40) -->[forty].
tens(50) -->[fifty].
tens(60) -->[sixty].
tens(70) -->[seventy].
tens(80) -->[eighty].
tens(90) -->[ninety].

test11() :- phrase(num(T), [five]), T = 5.
test12() :- phrase(num(T), [sixteen]), T = 16.
test13() :- phrase(num(T), [twenty]), T = 20.
test14() :- phrase(num(T), [twenty,five]), T = 25.
test15() :- phrase(num(T), [two, hundred]), T = 200.
test16() :- phrase(num(T), [two, hundred, two]), T = 202.
test17() :- phrase(num(T), [two, hundred, ten]), T = 210.
test18() :- phrase(num(T), [two, hundred, twenty]), T = 220.
test19() :- phrase(num(T), [two, hundred, twenty, two]), T = 222.
test20() :- phrase(num(T), [two, thousand]), T = 2000.
test21() :- phrase(num(T), [two, thousand, two]), T = 2002.
test22() :- phrase(num(T), [two, thousand, eleven]), T = 2011.
test23() :- phrase(num(T), [two, thousand, twenty]), T = 2020.
test24() :- phrase(num(T), [two, thousand, twenty, two]), T = 2022.
test25() :- phrase(num(T), [two, thousand, two, hundred]), T = 2200.
test26() :- phrase(num(T), [two, thousand, two, hundred, two]), T = 2202.
test27() :- phrase(num(T), [two, thousand, two, hundred, ten]), T = 2210.
test28() :- phrase(num(T), [two, thousand, two, hundred, twenty]), T = 2220.
test29() :- phrase(num(T), [two, thousand, two, hundred, twenty, two]), T = 2222.
test30() :- phrase(num(25), [twenty,five]).
test31() :- phrase(num(0), [zero]).
test32() :- phrase(num(2000), [two, thousand]).
test33() :- phrase(num(2345), L), L = [two, thousand, three, hundred, forty, five].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
t(E, Lt, Rt).
b_t(nil) --> [].
b_t(t(E, Lt, Rt)) --> b_t(Lt), b_t(Rt).
in_order(T, L).
in_order(t(E, Lt, Rt), L) --> in_order(Lt,LL), in_order(Rt, RL), append(LL, [E|LR], L).
in_order(nil,[]).
%in_oder
nodes(nil) --> [].
nodes(t(E,Lt,Rt)) --> nodes(Lt), [E], nodes(Rt).
%%%
post_order(nil, []).
%post_order(T, L).
post_order(t(E, Lt, Rt), L) :- post_order(Lt, LL), post_order(Rt, RL), append(LL, RL, L1), append(L1, [E], L).
%%post_order(t(a, t(b, nil, t(c, nil, nil)), t(d, nil, nil)), T).


test90() :- post_order(t(a, t(b, nil, t(c, nil, nil)), t(d, nil, nil)), T), T = [c, b, d, a].
test91() :- post_order(t(a, t(b, nil, t(c, nil, nil)), t(d, nil, nil)), [c, b, d, a]).
test92() :- post_order(nil, []).
test93() :- post_order(T, [c, b, d, a]), T = t(a, nil, t(d, nil, t(b, nil, t(c, nil, nil)))).
test94() :- post_order(T, L), T = nil, L = [].
test95() :- post_order(T, L), T = t(X, nil, nil), L = [X].

















