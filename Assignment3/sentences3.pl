% "Learn Prolog Now!"  Exercise 2.3
% Rewritten using Definite Clause Grammar rules
% Parsing and extracting a verb from the sentence

sentence(V) --> subject, verb(V), subject.
sentence(V) --> subject, verb(V).
subject --> article, noun.
article --> [a] | [every].
noun --> [criminal] | ['big kahuna burger'] | [dog].
verb(eats) --> [eats].
verb(likes) --> [likes].
