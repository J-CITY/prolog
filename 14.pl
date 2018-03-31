
queue(L) :- queue(L, Q-Q).

queue([e(X)|T], L) :- enq(X, L, L1), queue(T, L1), 
	write("Insert: "), write(X), write("->"), writeln(L1).

queue([d(X)|T], L) :- deq(X, L, L1), queue(T, L1), 
	write("Remove: "), write(X), write("->"), writeln(L1).

queue([], L).

enq(X, H-[X|T], H-T).
deq(X, [X|H]-T, H-T).

%queue([e(1), e(2),e(3),d(X), d(Y)]).
