%-------------------------------------------------------
% Programming Languages CSCI 4430
% Programming Assignment 3 Part 1
% Shawn George (georgs2) and Chrisanna Shen (shenc6)
%-------------------------------------------------------

% Person and Sibling are children of the same parent
% Person and Sibling cannot be the same person
siblings(Person, Sibling) :-
	parentOf(X,Person), parentOf(Y,Sibling), X=Y, Person\=Sibling.

% one of Person's parents is siblings with one of Cousin's parents
firstCousins(Person, Cousin) :-
	parentOf(X,Person), parentOf(Y,Cousin), siblings(X,Y), Person\=Cousin.

% Person is either a child, grandchild, great-grandchild, et cetera, of Ancestor
hasAncestor(Person, Ancestor) :-
	parentOf(Ancestor, Person).
hasAncestor(Person, Ancestor) :-
    parentOf(X, Person), hasAncestor(X, Ancestor).

% Person is either a parent, grandparent, great-grandparent, et cetera, of Descendant 
% Person is an ancestor of Descendant
hasDescendant(Person, Descendant) :-
	hasAncestor(Descendant, Person).

% using findall to get the list of all of the people with the ancestor Ancestor
listAncestors(Person, Ancestors) :-
	findall(Ancestor, hasAncestor(Person,Ancestor), Ancestors).

% useing findall to get the list of all the descendants of Person
listDescendants(Person, Descendants) :-
	findall(Descendant, hasDescendant(Person,Descendant), Descendants).

% Person must be monarch, and oldest checks if Heir is the Person's oldest child.
hasHeir(Person, Heir) :-
	monarch(Person), 
	findall(Child, parentOf(Person, Child), Children), 
	childOf(Heir, Person),
	birthYear(Heir, HeirYear),
	oldest(Person, Children, Heir, HeirYear).

% oldest/4 checks if the birth year of the person passed in is the earliest of 
% the parents' children, making them the oldest.
oldest(_, [], _, _).
oldest(Parent, [Sibling|Rest], Child, Year):-
	birthYear(Sibling, SibYear), SibYear >= Year, oldest(Parent, Rest, Child, Year).

% if Person was a monarch, their successor is any of their children who also were/are a monarch.
hasSuccessor(Person, Successor) :-
	childOf(Successor, Person),
	monarch(Person),
	monarch(Successor).

% Person's original heir ended up succeeding them
heirIsSuccessor(Person) :-
	hasHeir(Person, Heir),
	hasSuccessor(Person, Successor),
	Heir = Successor. 
