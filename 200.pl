sent(snt(NP, VP)) --> noun-ph(NP), verb_ph(VP).
noun_ph(np(D,N)) --> det(D), noun_ph(N).

noun_ph(np(N)) --> noun_ph(N).

noun_ph(np2(A,N)) --> adj(A), noun_ph2(N).
noun_ph(np2(N)) --> noun(N).

verb_ph(vp(V)) --> verb(V).
verb_ph(vp(V,N)) --> verb(V), noun-ph(N).

det(d(the)) --> [the].
det(d(a)) --> [a].
naun(n(pieplate)) --> [pieplate].
adj(a(decorated)) --> [decorated].
verb(v(contain)) --> [contain].



%noun(pieplate, s) --> [pieplate].
%noun(pieplates, p) --> [pieplates].
%sent(sen(NP, VP)) --> noun-ph(Np, Num), verb_ph(VP, Num).
%noun-ph(np(D, N), Num) --> det(D, Num), noun-ph(N, Num).
%verb_ph(vp(V, N), Num) --> verb(v(V, Num)), naun_ph(np(N, Nums)).


num(0) --> [zero].
num(N) --> n3(N).
n3(N) --> digit(D), [hundreed], rest_n3(N1), N is D*100+N1.
n3(N) --> n21(N)
rest_n3(0) --> [].
rest_n3(N) --> n21(N).
n21(N) --> digit(N). 
n21(N) --> teen(N). 
n21(N) --> rest(T) ,rest21(N1), N is T + N1.
rest21()) --> [zero].
rest21(N) --> digit(N).

digit(1) --> [one].

digit(9) --> [nine].

teen-->[eleven].

tens(10) -->[ten].



if --> true, then(A), else.
then(A) --> A.
then --> [].
if --> false, then else(A).
else(A) --> A.
esle --> [].

%%%%%%%%%%%%%%%%

t(E, Lt, Rt).
b_t(nil).
b_t(t(E, Lt,Bt)) :- b_t(Lt), b_t(Rt).

in_order(T, L).
in_order(t(E, Lt, Rt), L) :- in_order(Lt,LL), in_order(Rt, RL), append(LL, [E|LR], L).
in_order(nil,[]).

pre_oder
post_oder
%in_oder

nodes(nil) --> [].
nodes(t(E,Lt,Rt)) --> nodes(Lt), [E], nodes(Rt).

%phrase(t(...), L).
%1 трасировка предл с ед и мн числом
%2 10000
%3 post_oder






