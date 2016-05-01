:- ['tabling.pl','testlib.pl','table_print.pl'].
:- ['format.pl'].

%%% EXAMPLE 4
% Two mutually recursive predicates:
% d(X) :- e(Y), Y < 5, X is Y + 1.
% d(0).
%
% e(X) :- d(Y), Y < 5, X is Y + 1.
% e(0).

expected_variants([d(_),e(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).

go :-
  once(d(_)).

d(X) :-
  start_tabling(d(X),d_aux(X)).

e(X) :-
  start_tabling(e(X),e_aux(X)).

d_aux(X) :- writeln('d_aux: before calling e(Y)'), e(Y), writeln('d_aux: after calling e(Y)'), format:format('d_aux: Y is ~w~n',[Y]), Y < 5, format:format('d_aux: Y < 5 OK\n',[]), ( X is Y + 1 -> format:format('d_aux: is OK\n',[]) ; format:format('d_aux: ~w is ~w + 1 NOT ok\n',[X,Y]) ), format:format('d_aux: X is ~w~n',[X]).
d_aux(0).

e_aux(X) :- writeln('e_aux: before calling d(Y)'), d(Y), writeln('e_aux: after calling d(Y)'), format:format('e_aux: Y is ~w~n',[Y]), Y < 5, format:format('e_aux: Y < 5 OK\n', []), ( X is Y + 1 -> format:format('e_aux: is OK\n',[]) ; format:format('e_aux: ~w is ~w + 1 NOT ok\n',[X,Y]) ), format:format('e_aux: X is ~w~n',[X]).
e_aux(0).
