:- ensure_loaded(['tabling.pl','testlib.pl','table_print.pl']).
:- ensure_loaded(['format.pl']).

%%% EXAMPLE A %%%%
% A simple example to test the version with mutual recursion.

expected_variants([p(a,_),p(b,_),p(_,_)]).
expected_answers_for_variant(p(_,_),[p(a,1),p(b,2),p(a,2),p(b,1)]).
expected_answers_for_variant(p(a,_),[p(a,1),p(a,2)]).
expected_answers_for_variant(p(b,_),[p(b,1),p(b,2)]).

compare_answers :-
  p(_,_).

% Expected answers:
%
% a,1
% b,2
% a,2
% b,1

go :-
  once(p(_X,_Y)).

p(X,Y) :-
  start_tabling(p(X,Y),p_aux(X,Y)).

p_aux(a,1).
p_aux(a,X) :-
  p(b,X), format('after in p_aux(a,X)~n',[]).
p_aux(b,2).
p_aux(b,X) :-
  p(a,X), format('after in p_aux(b,X)~n',[]).
