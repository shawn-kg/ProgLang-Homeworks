:- use_module(library(clpfd)).
:- use_module(library(dcg/basics)).

:- [read_line].

read_loop :-
	read_line(Line),
	length(Line, Len),
	(
		Len > 0,
		write(Line), nl,
		% % definition(I,C,A,D,Line,[]),
		% write(I), nl,
		% write(C), nl,
		% write(A), nl,
		% write(D), nl,
		read_loop;
		Len =< 0,
		write('Read last line.'), nl
	).


definition(I,C,A,D) -->  two_words, id(I), one_word, core(C), three_words, area(A), five_words, dollars(D), two_words.
% instead of id(I) do : char_type(I, alpha), id(I)
one_word --> word.
two_words --> word, word.
three_words --> word, word, word.
five_words --> word, word, word, word, word.

id(a) --> [a].
core(16) --> [16].
area(4) --> [4].
dollars(1000) --> [1000].

word --> [processor] | [type] | [has] | [cores] | [','] | [square] | [centimeters] | [and] | [costs] | [dollars] | ['.'] | [uses].

constraint(Attr,Comp) --> attribute(Attr), imperative, [be], comparison(Comp), value.
constraint(Attr,RangeMin,RangeMax) --> attribute(Attr), imperative, [be], interval, range(RangeMin,RangeMax).

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

value --> integer. % how do i do this
interval --> [in], [the], intv_choices, [of].
intv_choices --> [interval] | [range].

range(RangeMin,RangeMax) --> range_min(RangeMin), [to], range_max(RangeMax). % closed aka inclusive
range_min --> integer.
range_max --> integer.

% constraint(Attr, Imp, X, Y) --> attribute(Attr), imperative(Imp), one_word, constr(X).	

% attribute(W) --> att_word(W).
% attribute(W, V) --> att_word(W), att_word(V).
% attribute(W, V, X) --> att_word(W), att_word(V), att_word(X).

% imperative(W) --> imp_word(W).
% imperative(W,V) --> imp_word(W), imp_word(V).

% comparison(W,V) --> comp_word(W), comp_word(V).

% att_word(the) --> [the]. 
% att_word(core) --> [core].
% att_word(count) --> [count].
% att_word(area) --> [area].
% att_word(cost) --> [cost].

% imp_word(must) --> [must].
% imp_word(is) --> [is].
% imp_word(to) --> [to].

% comp_word(equal) --> [equal].
% comp_word(less) --> [less].
% comp_word(greater) --> [greater].
% comp_word(than) --> [than].
% comp_word(to) --> [to].

include(Goal, List, Included) :-
	phrase(include_(List, Goal), Included).

include_([], _) --> [].
include_([L|Ls], Goal) -->
	(   { call(Goal, L) } ->
		[L]
	;   []
	),
	include_(Ls, Goal).

%%%%%%%%


% integer(I) -->
% 	digit(D0),
% 	digits(D),
% 	{ number_codes(I, [D0|D])
% 	}.

% digits([D|T]) -->
% 	digit(D), !,
% 	digits(T).
% digits([]) -->
% 	[].

% digit(D) -->
% 	[D],
% 	{ code_type(D, digit)
% 	}.



main :-
	read_loop.










	

