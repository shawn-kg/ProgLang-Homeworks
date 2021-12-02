siblings(Person, Sibling) :-
	parentOf(X,Person), parentOf(Y,Sibling), X=Y, Person\=Sibling.

firstCousins(Person, Cousin) :-
	parentOf(X,Person), parentOf(Y,Cousin), siblings(X,Y), Person\=Cousin.

hasAncestor(Person, Ancestor) :-
	parentOf(Ancestor, Person).
hasAncestor(Person, Ancestor) :-
    parentOf(X, Person), hasAncestor(X, Ancestor).

hasDescendant(Person, Descendant) :-
	hasAncestor(Descendant, Person).

listAncestors(Person, Ancestors) :-
	findall(Ancestor, hasAncestor(Person,Ancestor), Ancestors).


listDescendants(Person, Descendants) :-
	findall(Descendant, hasDescendant(Person,Descendant), Descendants).

hasHeir(Person, Heir) :-
	monarch(Person), 
	findall(Child, parentOf(Person, Child), Children), 
	childOf(Heir, Person),
	birthYear(Heir, HeirYear),
	oldest(Person, Children, Heir, HeirYear).

oldest(_, [], _, _).
oldest(Parent, [Sibling|Rest], Child, Year):-
	birthYear(Sibling, SibYear), SibYear >= Year, oldest(Parent, Rest, Child, Year).


hasSuccessor(Person, Successor) :-
	childOf(Successor, Person),
	monarch(Person),
	monarch(Successor).

heirIsSuccessor(Person) :-
	hasHeir(Person, Heir),
	hasSuccessor(Person, Successor),
	Heir = Successor. 
