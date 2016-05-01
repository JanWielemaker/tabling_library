:- ['tabling.pl','testlib.pl','table_print.pl'].
:- ['format.pl'].

% Expected answers:
% d(0), d(2), d(4)
% e(1), e(3), e(5)

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0),d(2),d(4)]).
expected_answers_for_variant(e(_),[e(1),e(3),e(5)]).

go :-
  once(d(_)).

d(X) :-
  start_tabling(d(X),d_aux(X)).

e(X) :-
  start_tabling(e(X),e_aux(X)).

d_aux(X) :- e(Y), Y < 5, X is Y + 1.
d_aux(0).
e_aux(X) :- d(Y), Y < 5, X is Y + 1.
% No facts for e
