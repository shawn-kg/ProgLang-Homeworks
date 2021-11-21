:- [read_line].

read_loop :-
	read_line(Line),
	length(Line, Len),
	(
		Len > 0,
		write(Line), nl,
		% definition(I,C,A,D,Line,[]),
		read_loop;
		Len =< 0,
		write('Read last line.'), nl
	).
	
definition(I,C,A,D) -->  two_words, id(I), one_word, core(C), three_words, area(A), five_words, dollars(D), two_words.

one_word --> word.
two_words --> word, word.
three_words --> word, word, word.
five_words --> word, word, word, word, word.

id(a) --> [a].
core(16) --> [16].
area(4) --> [4].
dollars(1000) --> [1000].

word --> [processor] | [type] | [has] | [cores] | [','] | [square] | [centimeters] | [and] | [costs] | [dollars] | ['.'] | [uses].
	



main :-
	read_loop.


	

