:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

% Smaller version of example 10.

% Expected answers

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0), d(2), d(5)]).
expected_answers_for_variant(e(_),[e(1)]).

go :-
  once(d(_)).

d(X) :-
  start_tabling(d(X),d_aux(X)).

e(X) :-
  start_tabling(e(X),e_aux(X)).

d_aux(X) :- e(Y), Y < 2, X is Y + 1.  % Will never run, because e doesn't have any facts (initially)
d_aux(X) :- d(Y), Y < 2 , X is Y + 5. % Should run
d_aux(0).
e_aux(X) :- d(Y), Y < 2, X is Y + 1.
% No facts for e
