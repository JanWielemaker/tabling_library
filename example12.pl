:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

expected_variants([f(4),f(3),f(2),f(0),f(1),e(_),d(_)]).
% Note: f(4) and f(0) are empty tables, but they are there...
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,4,X),L).
expected_answers_for_variant(f(4),[]).
expected_answers_for_variant(f(3),[f(3)]).
expected_answers_for_variant(f(2),[f(2)]).
expected_answers_for_variant(f(0),[]).
expected_answers_for_variant(f(1),[f(1)]).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,3,X),L).

% Expected answers
%
% d(0)
% d(1)
% d(2)
% d(3)
% d(4)
%
% e(0)
% e(1)
% e(2)
% e(3)
%
% f(1)
% f(2)
% f(3)

go :-
  once(d(_)).

d(X) :-
  start_tabling(d(X),d_aux(X)).

e(X) :-
  start_tabling(e(X),e_aux(X)).

f(X) :-
  start_tabling(f(X),f_aux(X)).

% Something like:
d_aux(X) :- e(Y), format:format('d_aux: after e(Y): ~w\n',[e(Y)]), X is Y + 1, X < 5.
d_aux(0).
% Number of predicates involved in mutual recursion will increase at runtime
e_aux(X) :- d(X), f(X), format:format('e_aux: at end of clause; head is now ~w\n',[e_aux(X)]).
e_aux(0) :- format:format('using fact e(0)\n',[]).
f_aux(X) :- e(Y), X is Y + 1, X < 4.
