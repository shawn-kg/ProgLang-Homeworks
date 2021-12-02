%-------------------------------------------------------
% Programming Languages CSCI 4430
% Programming Assignment 3 Part 2
% Shawn George (georgs2) and Chrisanna Shen (shenc6)
%-------------------------------------------------------

% libraries used
:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).
:- use_module(library(lists)).

:- [read_line].

% reads the input line by line, parsing with definition/6 and constraint/5 and appending them to lists
% all the possible processor combinations that satisfy the input constrain sentences are identified 
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



% processor definition sentence syntax
definition(I,C,A,D) -->  two_words, id(I), one_word, core(C), three_words, area(A), five_words, dollars(D), two_words.

one_word --> word.
two_words --> word, word.
three_words --> word, word, word.
five_words --> word, word, word, word, word.

% checking and binding relevant values (id, core count, area, and cost) to variables.
id(I) --> [I], {is_alpha(I)}. 
core(C) --> [C], {integer(C)}.
area(A) --> [A], {integer(A)}.
dollars(D) --> [D], {integer(D)}.

% acceptable words used in definition sentences
word --> [processor] | [type] | [has] | [cores] | [','] | [square] | [centimeters] | [and] | [costs] | [dollars] | ['.'] | [uses].

% constraint sentence definition syntax
constraint(Attr,Comp,V) --> attribute(Attr), imperative, [be], comparison(Comp), value(V), ['.'].
constraint(Attr,RangeMin,RangeMax) --> attribute(Attr), imperative, [be], interval, range_min(RangeMin), [to], range_max(RangeMax), ['.'].

% constraint attribute definition
attribute(Attr) --> attr_first, attr_second(Attr), attr_third.
attr_first --> [the] | [].
attr_second(core) --> [core].
attr_second(area) --> [area].
attr_second(cost) --> [cost].
attr_third --> [count] | [].

% constraint imperative definition
imperative --> imp_first, imp_second.
imp_first --> [must] | [is].
imp_second --> [to] | [].

% constraint comparison term definition
comparison(Comp) --> comp_first(Comp), comp_second.
comp_first(equal) --> [equal].
comp_first(less) --> [less].
comp_first(greater) --> [greater].
comp_second --> [to] | [than].

% binding constraint value to variable
value(V) -->  [V], { integer(V) }.

% constraint interval phrase definition
interval --> [in], [the], intv_choices, [of].
intv_choices --> [interval] | [range].

% constrain interval values binded to variables
range_min(RangeMin) --> [RangeMin], { integer(RangeMin) }.
range_max(RangeMax) --> [RangeMax], { integer(RangeMax) }.


% Evaluates constraint, depending on if constraint attribute is core, area, or cost. 

% Possible values are between 0 to 16 processors (inclusive), so a list with length of the number 
% of processor definitions will contain this domain as each element. This list of 0..16 is passed to
% helper functions to check if the combination of number of corresponding processors satisfy the constraints.
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

% the number of each processor and their ids are passed to output/4 to print out
answers(ProcList, [Answers | Tail]):-
	getIds(ProcList, Ids, 0),
	length(Ids, Anslen),
	output(Ids,Answers, Anslen, 0),
	answers(ProcList, Tail).
answers(_,[]).

% writes solution to terminal
output([],[],_, _) :-
	write(';'), nl.
output([H1|T1],[H2|T2],L, Counter) :-
	write(H1), write(' = '), write(H2),
	NewCounter is Counter+1,
	(checkCounter(NewCounter, L) ; true),
	output(T1, T2, L, NewCounter).

% helper function to check when to add comma (as opposed to a semicolon)
checkCounter(Counter, L) :-
	(Counter<L -> write(', ')).

% stores the processors' id in a list
getIds([],_,_).
getIds([H|T], [NHH|NHT], Ind) :-
	nth0(Ind,H,X),
	NHH = X, 
	getIds(T,NHT,Ind).

% helper function used in checking constraints
% used to get a list of constraint values corresponding to the index passed in
getValues([],_,_).
getValues([H|T], [NHH|NHT], Ind) :-
	nth0(Ind,H,X),
	NHH is X,
	getValues(T,NHT,Ind).

% helper function used in checking constraints
% used to get the sum of a certain combination of constraint property values
% this is later compared with the input constraints to check satisfaction
multList([],[],0).
multList([H1|T1], [H2|T2], Value) :-
	Temp = H1*H2,
	multList(T1,T2, Rest),
	Value = Temp + Rest.

% main
main :-
	read_loop([], []).

