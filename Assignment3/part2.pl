:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

:- [read_line].

read_loop(ProcList,ConstList) :-
	read_line(Line),
	length(Line, Len),
	(
		Len > 0,
		((definition(I,C,A,D,Line,[]),
		append(ProcList,[[I,C,A,D]],ProcNewList),
		append(ConstList, [], ConstNewList)
		); 
		(constraint(Attr, Comp, V, Line, []),
		append(ConstList, [[Attr, Comp, V]], ConstNewList),
		append(ProcList, [], ProcNewList)
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


id(I) --> [I], {is_alpha(I)}. 
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
	((Attr=core ->
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

