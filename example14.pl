:- ['tabling.pl','testlib.pl','table_print.pl'].
:- ['format.pl'].

% Simpler example than example12.pl, but the number of predicates involved in mutual recursion will also increase at runtime.

expected_variants([p(3,_),p(2,_),q(2,_),q(3,_),p(_,_)]).
% Note: p(3,_) and q(3,_) are empty tables, but they are there.
expected_answers_for_variant(p(_,_),[p(1,2),p(2,3),p(1,3)]).
expected_answers_for_variant(p(3,_),[]).
expected_answers_for_variant(p(2,_),[p(2,3)]).
expected_answers_for_variant(q(2,_),[q(2,3)]).
expected_answers_for_variant(q(3,_),[]).

go :-
  once(p(_X,_Y)).

p(X,Y) :- start_tabling(p(X,Y),p_aux(X,Y)).
q(X,Y) :- start_tabling(q(X,Y),q_aux(X,Y)).

p_aux(X,Y) :- p(X,Z), q(Z,Y).
p_aux(X,Y) :- e(X,Y).
q_aux(X,Y) :- p(X,Y).

e(1,2).
e(2,3).
