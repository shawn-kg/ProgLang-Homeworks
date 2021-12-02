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
		findall(Res, (constraints(ProcList,ConstList,Res), label(Res)), Answers),
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



constraints(_,[],_).
constraints(ProcList,[[Attr, X, Y]|T],Processors) :-
	length(ProcList, L), length(Processors, L), Processors ins 0..16, 
	% Processors = [Processor1, Processor2, Processor3, Processor4],
	((Attr=core ->
	% nth0(0,ProcList, A),
	% nth0(1,ProcList, B),
	% nth0(2,ProcList, C),
	% nth0(3,ProcList, D),

	% nth0(1,A,CoreConst1),
	% nth0(1,B,CoreConst2),
	% nth0(1,C,CoreConst3),
	% nth0(1,D,CoreConst4),
	% (Processor1*CoreConst1 + Processor2*CoreConst2 + Processor3*CoreConst3 + Processor4*CoreConst4)
	getValues(ProcList,Cores,1),
	multList(Processors, Cores, Coresum), 
	((X = greater -> Coresum #> Y);
	(X = less ->  Coresum #< Y);
	(X = equal -> Coresum #= Y);
	(integer(X) -> Coresum #>= X, 
	Coresum #=< Y)), 
	constraints(ProcList,T,Processors));
	
	(Attr=area ->  
	getValues(ProcList,Areas,2),
	multList(Processors, Areas, Areasum), 
	((X = greater -> Areasum #> Y);
	(X = less -> Areasum #< Y);
	(X = equal ->  Areasum #= Y);
	(integer(X) -> Areasum #>= X, 
	Areasum #=< Y)), 
	constraints(ProcList,T,Processors));
	
	(Attr=cost ->
	getValues(ProcList,Costs,3),
	multList(Processors, Costs, Costsum), 
	((X = greater -> Costsum #> Y);
	(X = less -> Costsum #< Y);
	(X = equal -> Costsum #= Y);
	(integer(X) -> Costsum #>= X,
	Costsum #=< Y)), 
	constraints(ProcList,T,Processors))).

answers(ProcList, [Answers | Tail]):-
	getIds(ProcList, Ids, 0),
	length(Ids, Anslen),
	output(Ids,Answers, Anslen, 0),
	answers(ProcList, Tail).
answers(_,[]).

output([],[],_, _) :-
	write(';'), nl.
output([H1|T1],[H2|T2],L, Counter) :-
	write(H1), write(' = '), write(H2),
	NewCounter is Counter+1,
	(checkCounter(NewCounter, L) ; true),
	output(T1, T2, L, NewCounter).

checkCounter(Counter, L) :-
	(Counter<L -> write(', ')).

getIds([],_,_).
getIds([H|T], [NHH|NHT], Ind) :-
	nth0(Ind,H,X),
	NHH = X, 
	getIds(T,NHT,Ind).


getValues([],_,_).
getValues([H|T], [NHH|NHT], Ind) :-
	nth0(Ind,H,X),
	NHH is X,
	getValues(T,NHT,Ind).

% Do this getValues(ProcList, AttrConst, Index).
% NewList is a list of Attribute constraint constants
	
multList([],[],0).
multList([H1|T1], [H2|T2], Value) :-
	Temp = H1*H2,
	multList(T1,T2, Rest),
	Value = Temp + Rest.

main :-
	read_loop([], []).

