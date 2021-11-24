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
		append(ConstList, [], ConstNewList)); 
		(constraint(Attr, Comp, V, Line, []),
		append(ConstList, [[Attr, Comp, V]], ConstNewList),
		append(ProcList, [], ProcNewList))),
		
		read_loop(ProcNewList, ConstNewList);
		Len =< 0,
		write(ProcList), nl,
		write(ConstList), nl,
		write('Read last line.'), nl
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



main :-
	read_loop([], []).










	

