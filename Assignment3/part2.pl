:- [read_line].

read_loop :-
	read_line(Line),
	length(Line, Len),
	(
		Len > 0,
		write(Line), nl,
		read_loop;
		Len =< 0,
		write('Read last line.'), nl
	).

main :-
	read_loop.