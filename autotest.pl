:- multifile autotest/1.

test :-
	forall(autotest(Goal),
	       call(Goal)).
