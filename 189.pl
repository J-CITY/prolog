translate([Rule|Rules],[Clause|Clauses]) :- translate_rule(Rule,Clause),translate(Rules,Clauses).

translate([ ],[ ]).

translate_rule((Lhs --> Rhs),(Head :- Body)) :- 
	translate_head(Lhs,Head,Xs-Ys),translate_body(Rhs,Body,Xs-Ys),!.

translate_head(A,A1,Xs) :- translate_goal(A,A1,Xs).

translate_body((A,{B}),(A1,B1),Xs-Ys) :-
	!, translate_body(A,A1,Xs-Ys), translate_body({B},B1,Ys).

translate_body((A,B),(A1,B1),Xs-Ys) :-
	!, translate_body(A,A1,Xs-Xs1), translate_body(B,B1,Xs1-Ys).
	
	
translate_body(({A}),A,Xs).
translate_body(A,A1,Xs) :- translate_goal(A,A1,Xs).

translate_goal(A,A1,DList) :- nonterminal(A), functor(A1,A,1), arg(1,A1,DList).
		   
translate_goal(Terms,connect(Terms,S),S) :- terminals(Terms).

translate_goal(T, R, S) :- functor(T,T1,X), X > 0, X1 is X+1, functor(R,T1,X1), arg(1, R, S), setargs(T, X1, R).


%setargs(T, X, R)
setargs(T, 1, R).
setargs(T, X, R) :- X0 is X-1, arg(X0, T, A), arg(X, R, A), setargs(T, X0, R).

%translate_goal({X}, X, S).

nonterminal(A) :- atom(A).
terminals(Xs) :- list(Xs).
list([]).
list([X|Xs]) :- list(Xs).

connect([],Xs-Xs).
connect([W|Ws],[W|Xs]-Ys) :-  connect(Ws,Xs-Ys).

%translate([(s(N) --> a(Na), bs(Nb), {N is Na+Nb}),(a(Na) --> [a], a(Na1), {Na is Na1 + 1}),(a(Na) --> [], {Na is 0}),(bs(Nb) --> [b], bs(Nb1), {Nb is Nb1 + 1}),(bs(Nb) --> [], {Nb is 0})], X).
%translate([(s(N) --> a(Na), bs(Nb), cs(Nc), {N is Na+Nb+Nc, Na=Nb, Nb=Nc}),(a(Na) --> [a], a(Na1), {Na is Na1 + 1}),(a(Na) --> [], {Na is 0}),(bs(Nb) --> [b], bs(Nb1), {Nb is Nb1 + 1}),(bs(Nb) --> [], {Nb is 0}),(cs(Nc) --> [c], cs(Nc1), {Nc is Nc1 + 1}),(cs(Nc) --> [], {Nc is 0})], X).
%translate([(append([]) --> []), (append([H|T]) --> [H], append(T))], X).

test0() :- translate(
	[
		(s(N) --> a(Na), bs(Nb), {N is Na+Nb}),
		(a(Na) --> [a], a(Na1), {Na is Na1 + 1}),
		(a(Na) --> [], {Na is 0}),
		(bs(Nb) --> [b], bs(Nb1), {Nb is Nb1 + 1}),
		(bs(Nb) --> [], {Nb is 0})
	], X), X = [
		(s(_3098-_3100, N):-a(_3098-_3124, Na), bs(_3124-_3100, Nb), N is Na+Nb),  
		(a(_3174-_3176, Na):-connect([a], _3174-_3200), a(_3200-_3176, Na1), Na is Na1+1),  
		(a(_3250-_3252, Na):-connect([], _3250-_3252), Na is 0),  
		(bs(_3302-_3304, Nb):-connect([b], _3302-_3328), bs(_3328-_3304, Nb1), Nb is Nb1+1),  
		(bs(_3378-_3380, Nb):-connect([], _3378-_3380), Nb is 0)
	].

test1() :- translate(
	[
		(s(N) --> a(Na), bs(Nb), {N is Na+Nb}),
		(a(Na) --> [a], a(Na1), {Na is Na1 + 1}),
		(a(Na) --> [], {Na is 0}),
		(bs(Nb) --> [b], bs(Nb1), {Nb is Nb1 + 1}),
		(bs(Nb) --> [], {Nb is 0})
	], 
	[
		(s(_3098-_3100, N):-a(_3098-_3124, Na), bs(_3124-_3100, Nb), N is Na+Nb),  
		(a(_3174-_3176, Na):-connect([a], _3174-_3200), a(_3200-_3176, Na1), Na is Na1+1),  
		(a(_3250-_3252, Na):-connect([], _3250-_3252), Na is 0),  
		(bs(_3302-_3304, Nb):-connect([b], _3302-_3328), bs(_3328-_3304, Nb1), Nb is Nb1+1),  
		(bs(_3378-_3380, Nb):-connect([], _3378-_3380), Nb is 0)
	]).

%X = [
%(s(_3098-_3100, N):-a(_3098-_3124, Na), bs(_3124-_3100, Nb), N is Na+Nb),  
%(a(_3174-_3176, Na):-connect([a], _3174-_3200), a(_3200-_3176, Na1), Na is Na1+1),  
%(a(_3250-_3252, Na):-connect([], _3250-_3252), Na is 0),  
%(bs(_3302-_3304, Nb):-connect([b], _3302-_3328), bs(_3328-_3304, Nb1), Nb is Nb1+1),  
%(bs(_3378-_3380, Nb):-connect([], _3378-_3380), Nb is 0)
%].

%X = [
%	(append(_4494-_4496, []):-connect([], _4494-_4496)),  
%	(append(_4530-_4532, [H|T]):-connect([H], _4530-_4556), append(_4556-_4532, T))
%].

append(_4494-_4496, []):-connect([], _4494-_4496).
append(_4530-_4532, [H|T]):-connect([H], _4530-_4556), append(_4556-_4532, T).


s(_558-_560, N):-a(_558-_584, Na), bs(_584-_608, Nb), cs(_608-_560, Nc), N is Na+Nb+Nc, Na=Nb, Nb=Nc.
a(_658-_660, Na):-connect([a], _658-_684), a(_684-_660, Na1), Na is Na1+1.
a(_734-_736, Na):-connect([], _734-_736), Na is 0.
bs(_786-_788, Nb):-connect([b], _786-_812), bs(_812-_788, Nb1), Nb is Nb1+1.  
bs(_862-_864, Nb):-connect([], _862-_864), Nb is 0.  
cs(_914-_916, Nc):-connect([c], _914-_918), cs(_918-_916, Nc1), Nc is Nc1 + 1.
cs(_972-_974, Nc):-connect([], _972-_974), Nc is 0.












