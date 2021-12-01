:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

:- [read_line].

read_loop(ProcList,ConstList) :-
	read_line(Line),
	length(Line, Len),
	(
		Len > 0,
		% write(Line), nl,
		((definition(I,C,A,D,Line,[]),
		append(ProcList,[[I,C,A,D]],ProcNewList),
		append(ConstList, [], ConstNewList)
		% assert(def(I,C,A,D))
		); 
		(constraint(Attr, Comp, V, Line, []),
		append(ConstList, [[Attr, Comp, V]], ConstNewList),
		append(ProcList, [], ProcNewList)
		% assert(const(Attr, Comp, V))
		)),
		
		read_loop(ProcNewList, ConstNewList);
		Len =< 0,
		write(ProcList), nl, write(ConstList), nl, 
		findall(Res, (constraints(ProcList,ConstList,Res)), label(Res), Answers),
		% constraints(ProcList,ConstList,Answers),
		write("finished findall"), nl,
		write(Answers),
		% write("reached here") , nl, 
		answers(ProcList,Answers)
	).




definition(I,C,A,D) -->  two_words, id(I), one_word, core(C), three_words, area(A), five_words, dollars(D), two_words.

one_word --> word.
two_words --> word, word.
three_words --> word, word, word.
five_words --> word, word, word, word, word.

id(a) --> [a].
id(b) --> [b].
id(c) --> [c].
id(d) --> [d].
id(e) --> [e].
id(f) --> [f].
id(g) --> [g].
id(h) --> [h].
id(i) --> [i].
id(j) --> [j].
id(k) --> [k].
id(l) --> [l].
id(m) --> [m].
id(n) --> [n].
id(o) --> [o].
id(p) --> [p].
id(q) --> [q].
id(r) --> [r].
id(s) --> [s].
id(t) --> [t].
id(u) --> [u].
id(v) --> [v].
id(w) --> [w].
id(x) --> [x].
id(y) --> [y].
id(z) --> [z].

core(C) --> [C], {integer(C)}.
area(A) --> [A], {integer(A)}.
dollars(D) --> [D], {integer(D)}.

word --> [processor] | [type] | [has] | [cores] | [','] | [square] | [centimeters] | [and] | [costs] | [dollars] | ['.'] | [uses].

constraint(Attr,Comp,V) --> attribute(Attr), imperative, [be], comparison(Comp), value(V), ['.'].
constraint(Attr,RangeMin,RangeMax) --> attribute(Attr), imperative, [be], interval, range_min(RangeMin), [to], range_max(RangeMax), ['.'].

attribute(Attr) --> attr_first, attr_second(Attr), attr_third.
attr_first --> [the] | [].
attr_second(core) --> [core].
attr_second(area) --> [area].
attr_second(cost) --> [cost].
attr_third --> [count] | [].

imperative --> imp_first, imp_second.
imp_first --> [must] | [is].
imp_second --> [to] | [].

comparison(Comp) --> comp_first(Comp), comp_second.
comp_first(equal) --> [equal].
comp_first(less) --> [less].
comp_first(greater) --> [greater].
comp_second --> [to] | [than].

value(V) -->  [V], { integer(V) }.

interval --> [in], [the], intv_choices, [of].
intv_choices --> [interval] | [range].

range_min(RangeMin) --> [RangeMin], { integer(RangeMin) }.
range_max(RangeMax) --> [RangeMax], { integer(RangeMax) }.

constraints(_,[],_):-
	write('finished constraints'), nl.
constraints(ProcList,[[Attr, X, Y]|T],Processors) :-
	length(Processors, 4), Processors ins 0..16, Processors = [Processor1, Processor2, Processor3, Processor4],
	write(Processors),
	(Attr=core ->
	nth0(0,ProcList, A),
	nth0(1,ProcList, B),
	nth0(2,ProcList, C),
	nth0(3,ProcList, D),

	nth0(1,A,CoreConst1),
	nth0(1,B,CoreConst2),
	nth0(1,C,CoreConst3),
	nth0(1,D,CoreConst4),
	((X = greater -> (Processor1*CoreConst1 + Processor2*CoreConst2 + Processor3*CoreConst3 + Processor4*CoreConst4) #> Y);
	(X =less ->  (Processor1*CoreConst1 + Processor2*CoreConst2 + Processor3*CoreConst3 + Processor4*CoreConst4) #< Y);
	(X = equal -> (Processor1*CoreConst1 + Processor2*CoreConst2 + Processor3*CoreConst3 + Processor4*CoreConst4) #= Y);
	(integer(X) -> (Processor1*CoreConst1 + Processor2*CoreConst2 + Processor3*CoreConst3 + Processor4*CoreConst4) ins X..Y)), 
	constraints(ProcList,T,Processors));
	
	(Attr=area ->  
	nth0(0,ProcList, A),
	nth0(1,ProcList, B),
	nth0(2,ProcList, C),
	nth0(3,ProcList, D),

	nth0(2,A,AreaConst1),
	nth0(2,B,AreaConst2),
	nth0(2,C,AreaConst3),
	nth0(2,D,AreaConst4),
	((X = greater -> (Processor1*AreaConst1 + Processor2*AreaConst2 + Processor3*AreaConst3 + Processor4*AreaConst4) #> Y);
	(X = less -> (Processor1*AreaConst1 + Processor2*AreaConst2 + Processor3*AreaConst3 + Processor4*AreaConst4) #< Y);
	(X = equal ->  (Processor1*AreaConst1 + Processor2*AreaConst2 + Processor3*AreaConst3 + Processor4*AreaConst4) #= Y);
	(integer(X) -> (Processor1*AreaConst1 + Processor2*AreaConst2 + Processor3*AreaConst3 + Processor4*AreaConst4) ins X..Y)), 
	constraints(ProcList,T,Processors));
	
	(Attr=cost ->
	nth0(0,ProcList, A),
	nth0(1,ProcList, B),
	nth0(2,ProcList, C),
	nth0(3,ProcList, D),

	nth0(3,A,CostConst1),
	nth0(3,B,CostConst2),
	nth0(3,C,CostConst3),
	nth0(3,D,CostConst4),
	((X = greater -> (Processor1*CostConst1 + Processor2*CostConst2 + Processor3*CostConst3 + Processor4*CostConst4) #> Y);
	(X = less -> (Processor1*CostConst1 + Processor2*CostConst2 + Processor3*CostConst3 + Processor4*CostConst4) #< Y);
	(X = equal -> (Processor1*CostConst1 + Processor2*CostConst2 + Processor3*CostConst3 + Processor4*CostConst4) #= Y);
	(integer(X) -> (Processor1*CostConst1 + Processor2*CostConst2 + Processor3*CostConst3 + Processor4*CostConst4) ins X..Y)), 
	constraints(ProcList,T,Processors)).

answers(ProcList, [Answers | Tail]):-
	nth0(0, ProcList, Proc1), nth0(1, ProcList, Proc2), nth0(2, ProcList, Proc3), nth0(3, ProcList, Proc4),
	nth0(0, Proc1, Id1), nth0(0, Proc2, Id2), nth0(0, Proc3, Id3), nth0(0, Proc4, Id4),
	Answers = [P1, P2, P3, P4],
	write(Id1), write(' = '), write(P1), write(', '),
	write(Id2), write(' = '), write(P2), write(', '),
	write(Id3), write(' = '), write(P3), write(', '),
	write(Id4), write(' = '), write(P4), write(';'),nl,
	answers(ProcList, Tail).
answers(_,[]).


main :-
	read_loop([], []).

% constraints(ProcList,[],P1,P2,P3,P4):-
	



% constraints(ProcList, [H|T]) :-
% 	nth0(0, H, Attr ), 
% 	(Attr=core, nth0(1, H, Comp), nth0(2, H, Val)
% 	(Comp=greater, ); 
% 	(Comp=less,);
% 	(Comp=equal,);
% 	(integer(Comp),)
% 	);
% 	(Attr=area, nth0(1, H, Comp), nth0(2, H, Val)
% 	(Comp=greater,); 
% 	(Comp=less,);
% 	(Comp=equal,);
% 	(integer(Comp),)
% 	);
% 	(Attr=cost, nth0(1, H, Comp), nth0(2, H, Val)
% 	(Comp=greater,); 
% 	(Comp=less,);
% 	(Comp=equal,);
% 	(integer(Comp),)
% 	);
% 	true.



% 	q(ProcessorList) :-
% 		nth0(0, H, Attr ), 
% 		(Attr=core, nth0(1, H, Comp), nth0(2, H, Val)
% 		(Comp=greater, ); 
% 		(Comp=less,);
% 		(Comp=equal,);
% 		(integer(Comp),)
% 		);
 

% %%%% scribble

% %coreCount(constraint, , C2) :- % constraint is an atom
% 	%if constrain is greater than


% 	constraints1(ProcList,[[core, X, Y]|T]):-
% 		Processor1 in 0..16, % x
% 		Processor2 in 0..16, 
% 		Processor3 in 0..16, 
% 		Processor4 in 0..16,
% 		nth0(0,ProcList, A), 
% 		nth0(1,ProcList, B),
% 		nth0(2,ProcList, C),
% 		nth0(3,ProcList, D),
% 		nth0(1,A,coreConst1),
% 		nth0(1,B,coreConst2),
% 		nth0(1,C,coreConst3),
% 		nth0(1,D,coreConst4),
% 		((X = greater, Processor1 *coreConst1 + Processor2*coreConst2 + Processor3* coreConst3 + Processor4*coreConst4 #> Y);
% 		(X = less, Processor1 *coreConst1 + Processor2*coreConst2 + Processor3* coreConst3 + Processor4*coreConst4 #< Y);
% 		(X = equal, Processor1 *coreConst1 + Processor2*coreConst2 + Processor3* coreConst3 + Processor4*coreConst4 #= Y);
% 		(integer(X), Processor1 *coreConst1 + Processor2*coreConst2 + Processor3* coreConst3 + Processor4*coreConst4 in X..Y)).
		
% 		% X is the list of 
% 		findall(NumProcessors, satisfyConstraints(X, ProcList, ConstList), Results)
			

% 		processorNum(P) :- between(0,16,P).

% 		checkConstraint([ProcX|ProcListT] :-
% 			processorNum(ProcX),
% 			checkConstraint(ProcListT, ConstListT)





	

