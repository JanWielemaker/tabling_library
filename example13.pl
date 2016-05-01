:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

% This is a simpler example than example5 that also contains a cycle. Test this first.

% This is a nice example for explaining mutual recursion, or how it can occur in a program that does not seem mutually recursive at first.

expected_variants([p(2,_),p(1,_),p(_,_)]).
expected_answers_for_variant(p(_,_),L) :-
  findall(p(X,Y),(between(1,2,X),between(1,2,Y)),L).
expected_answers_for_variant(p(2,_),L) :-
  findall(p(2,X),between(1,2,X),L).
expected_answers_for_variant(p(1,_),L) :-
  findall(p(1,X),between(1,2,X),L).

% The answers we expect for example 13, returned as a list with entries of the form X-Y. The order does not matter.
% We expect four answers.
p_expected_answers([1-2,2-1,1-1,2-2]).

go :-
  once(p(_X,_Y)).

p(X,Y) :-
  start_tabling(p(X,Y),p_aux(X,Y)).

p_aux(X,Y) :-
  writeln('before'), p(X,Z), writeln('between'), p(Z,Y).
p_aux(X,Y) :- e2(X,Y).

e2(1,2).
e2(2,1).
