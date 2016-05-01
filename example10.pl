:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

%d(X) <- e(Y), Y < 5, X is Y + 1.  % Will never run, because e doesn't have any facts
%d(X) <- d(Y), Y < 20, X is Y + 5. % Should run
%d(0).
%e(X) <- d(Y), Y < 5, X is Y + 1.
%% No facts for e

% Expected answers [TODO: recalculate by hand]
% d(0)
% d(2)
% d(4)
% d(5)
% d(7)
% d(9)
% d(10)
% d(12)
% d(14)
% d(15)
% d(17)
% d(19)
% d(20)
% d(22)
% d(24)
% 
% e(5)
% e(3)
% e(1)

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),[d(0),d(2),d(4),d(5),d(7),d(9),d(10),d(12),d(14),d(15),d(17),d(19),d(20),d(22),d(24)]).
expected_answers_for_variant(e(_),[e(5),e(3),e(1)]).

go :-
  once(d(_)).

d(X) :-
  start_tabling(d(X),d_aux(X)).

e(X) :-
  start_tabling(e(X),e_aux(X)).

d_aux(X) :- e(Y), Y < 5, X is Y + 1.  % Will never run, because e doesn't have any facts (initially)
d_aux(X) :- d(Y), Y < 20, X is Y + 5. % Should run
d_aux(0).
e_aux(X) :- d(Y), Y < 5, X is Y + 1.
% No facts for e
