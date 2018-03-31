

initial(q0).
final(q0).
delta(q0, a, q1).
delta(q1, b, q0).
accept(L) :- initial(Q), accept(L,Q).
accept([H|T], Q) :- delta(Q, H, Q1), accept(T, Q1). 
accept([], Q) :- final(Q).
%%%%%%%%%%%%%%%%%%%%%%%%
preduce(true, true) :- !.

preduce((A,B), C) :- !,preduce(A, PA), preduce(B, PB), combine(PA, PB, C).

preduce(A, B) :- s_fold(A,B), !.

preduce(A, R) :- s_unfold(A), !, clause(A, B), preduce(B, R).

%preduce(A, A):-.
preduce(A:-B, (PA:-PB)) :- !, preduce(B, PB), preduce(A, PA).

s_unfold(initial(Q)).
s_unfold(final(Q)).

s_unfold(delta(Q, H, Q1)).


s_fold(accept(L), goodstr(L)).
s_fold(accept(L, Q), goodstr(L, Q)).

combine(A, true, A) :- !.
combine(true, A, A) :- !.
combine(A, B, (A,B)).

process(P, NewP) :- findall(PC1, (member(C1, P), preduce(C1, PC1), writeln(PC1)), NewP).

%process([(initial(q0).),(final(q0).),(delta(q0, a, q1).),(delta(q1, b, q0).),(accept(L) :- initial(Q), accept(L,Q).),(accept([H|T], Q) :- delta(Q, H, Q1), accept(T, Q1).),(accept([], Q) :- final(Q).)]).

%delta1(q0, a, S, q1, [a, S]).
%delta1(q1, b, [a|T], q0, T).
%
%accept1(L) :- initial(Q), accept1(L, Q, []).
%
%accept1([H|T], Q, S) :- delta1(Q, H, S, Q1, S1), accept1(T, Q1, S1).
%
%accept1([], Q, []) :- final(Q).


