:- ['tabling.pl','testlib.pl','table_print.pl'].
:- use_module(library(format)).

% Example designed to test whether the true in fresh status works fine in case only a continuation is saved.
% See issue 55.

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).

d(X) :- start_tabling(d(X),d_aux(X)).
e(X) :- start_tabling(e(X),e_aux(X)).

go :-
  once(d(_)).

d_aux(X) :- e(Y), Y < 5, X is Y + 1.
d_aux(0).
e_aux(X) :- d(Y), e(_), Y < 5, X is Y + 1.
e_aux(0).
