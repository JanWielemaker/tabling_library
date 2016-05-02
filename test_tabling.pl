:- module(test_tabling,
	  [ test_tabling/0
	  ]).
:- use_module(tabling).
:- use_module(testlib).
:- use_module(testlib2).
:- use_module(library(plunit)).

test_tabling :-
	run_tests([ tabling_ex1,
		    tabling_ex2,
		    tabling_ex3,
		    tabling_ex4,
		    tabling_ex6,
		    tabling_ex7
		  ]).

		 /*******************************
		 *	    EXAMPLE 1		*
		 *******************************/

:- begin_tests(tabling_ex1, [cleanup(abolish_all_tables)]).

expected_variants([a(2,_),a(3,_),a(_,_)]).
% Note: a(3,_) is an empty table, but it is there...
expected_answers_for_variant(a(_,_),[a(1,2),a(2,3),a(1,3)]).
expected_answers_for_variant(a(2,_),[a(2,3)]).
expected_answers_for_variant(a(3,_),[]).

a_expected_answers([1-2,2-3,1-3]).

a_compare_answers :-
  compare_real_expected_answers(a,2,a_expected_answers),
  test_expected_variants_present,
  test_answers_expected_tables.

:- table a/2.

a(X,Y) :- before, a(X,Z), between, a(Z,Y).
a(X,Y) :- e(X,Y).

e(1,2).
e(2,3).

test(ex1) :-
	a_compare_answers.

:- end_tests(tabling_ex1).

		 /*******************************
		 *           EXAMPLE 2		*
		 *******************************/

:- begin_tests(tabling_ex2, [cleanup(abolish_all_tables)]).
% Meerdere recursieve calls met zelfde callpattern:
% b(X,Y) :- b(X,Z), b(Q,Y).
% For two given facts e(1,2) and e(2,3), there are four (!) solutions to b(X,Y):
% 1,2
% 2,3
% 1,3
% 2,2

expected_variants([b(_,_)]).
expected_answers_for_variant(b(_,_),[b(1,2),b(2,3),b(1,3),b(2,2)]).

% The answers we expect for example 2, returned as a list with entries of the form X-Y. The order does not matter.
b_expected_answers([1-2,2-3,1-3,2-2]).

% TEST: Tests answers of example 2.
b_compare_answers :-
  compare_real_expected_answers(b,2,b_expected_answers).

go :-
  once(b(_X,_Y)).

:- table b/2.

b(X,Y) :- before, b(X,_Z), between, b(_Q,Y).
b(X,Y) :- e(X,Y).

% Test facts
e(1,2).
e(2,3).

test(ex2) :-
	b_compare_answers,
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex2).

		 /*******************************
		 *           EXAMPLE 3		*
		 *******************************/

:- begin_tests(tabling_ex3, [cleanup(abolish_all_tables)]).
% Answers: all integers between -10 and 10 (included).
%
% Consider the execution of the query {\tt ?- p$_g$(Y).} which
% succeeds for all integers between {\tt -10} and {\tt 10} (included)
% against the following tabled program:
% :- table p/1. \\
% p(X) :- p$_{c_1}$(Y), 0 =< Y, Y < 10, X is -Y - 1.  \\
% p(X) :- p$_{c_2}$(Y), -10 < Y, Y =< 0, X is -Y + 1. \\
% p(0).
% The two consumers that are encountered have been given an index for
% ease of reference.  The abstract machine needs to alternate between
% consumers {\tt p$_{c_1}$(Y)} and {\tt p$_{c_2}$(Y)} multiple times
% before all answers have been generated.

expected_variants([c(_)]).
expected_answers_for_variant(c(_),L) :-
  findall(c(X),between(-10,10,X),L).

c_expected_answers(L) :-
  findall(X,between(-10,10,X),L).

c_compare_answers :-
  compare_real_expected_answers(c,1,c_expected_answers).

:- table c/1.

c(X) :- before(1), c(Y), feedback('after recusive 1: Y is ~w, and X is ~w',[Y,X]),
	0 =< Y, Y < 10, X is -Y-1, end(1).
c(X) :- before(2), c(Y), feedback('after recusive 2: Y is ~w, and X is ~w',[Y,X]),
	-10 < Y, Y =< 0, X is -Y+1, end(2).
c(0).

test(ex3) :-
	c_compare_answers,
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex3).


		 /*******************************
		 *           EXAMPLE 4		*
		 *******************************/

:- begin_tests(tabling_ex4, [cleanup(abolish_all_tables)]).
% Two mutually recursive predicates:
% d(X) :- e(Y), Y < 5, X is Y + 1.
% d(0).
%
% e(X) :- d(Y), Y < 5, X is Y + 1.
% e(0).

expected_variants([d(_), e(_)]).
expected_answers_for_variant(d(_),L) :-
  findall(d(X),between(0,5,X),L).
expected_answers_for_variant(e(_),L) :-
  findall(e(X),between(0,5,X),L).

de_expected_answers(L) :-
  findall(X,between(0,5,X),L).

:- table d/1, e/1.

d(X) :-
	feedback('d/1: before calling e(Y)'),
	e(Y),
	feedback('d/1: after calling e(Y)'),
	feedback('d/1: Y is ~w~n',[Y]),
	Y < 5,
	feedback('d/1: Y < 5 OK\n'),
	(   X is Y + 1
	->  feedback('d/1: is OK\n')
	;   feedback('d/1: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('d/1: X is ~w~n', [X]).
d(0).

e(X) :-
	feedback('e/1: before calling d(Y)'),
	d(Y),
	feedback('e/1: after calling d(Y)'),
	feedback('e/1: Y is ~w~n',[Y]),
	Y < 5,
	feedback('e/1: Y < 5 OK\n', []),
	(   X is Y + 1
	->  feedback('e/1: is OK\n',[])
	;   feedback('e/1: ~w is ~w + 1 NOT ok\n',[X,Y])
	),
	feedback('e/1: X is ~w~n',[X]).
e(0).

test(ex4) :-
  compare_real_expected_answers(d,1,de_expected_answers),
  compare_real_expected_answers(e,1,de_expected_answers),
  test_expected_variants_present,
  test_answers_expected_tables.

:- end_tests(tabling_ex4).


		 /*******************************
		 *           EXAMPLE 5		*
		 *******************************/

:- begin_tests(tabling_ex5, [cleanup(abolish_all_tables)]).
:- end_tests(tabling_ex5).


		 /*******************************
		 *           EXAMPLE 6		*
		 *******************************/

:- begin_tests(tabling_ex6, [cleanup(abolish_all_tables)]).
% Nested tabling: a tabled predicate calls another tabled predicate, but not mutually recursive.
% g(X) <- h(Y), X is Y + 1.
%
% h(X) <- h(Y), X is Y + 1, X < 5.
% h(0).

expected_variants([g(_),h(_)]).
expected_answers_for_variant(g(_),L) :-
  findall(g(X),between(1,5,X),L).
expected_answers_for_variant(h(_),L) :-
  findall(h(X),between(0,4,X),L).

g_expected_answers([1,2,3,4,5]).
h_expected_answers([0,1,2,3,4]).

:- table g/1, h/1.

g(X) :-
	feedback('g_aux: before calling h(Y)'),
	h(Y),
	feedback('g_aux: after calling h(Y)'),
	X is Y + 1.

h(X) :-
	feedback('h_aux: before calling h(Y)'),
	h(Y),
	feedback('h_aux: after calling h(Y)'),
	X is Y + 1,
	X < 5.
h(0).

test(ex6) :-
	compare_real_expected_answers(g,1,g_expected_answers),
	compare_real_expected_answers(h,1,h_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex6).


		 /*******************************
		 *           EXAMPLE 7		*
		 *******************************/

:- begin_tests(tabling_ex7, [cleanup(abolish_all_tables)]).
% A more complicated graph example:
%
%       i1
%       |
%    +--+---+
%    |      |
%    v      v
%    i2     i3
%    |
% +--+---+
% |      |
% v      v
% i4 --> i5
%
% i1(X) :- i2(Y), X is Y + 1.
% i1(X) :- i3(Y), X is Y + 1.
%
% i2(X) :- i4(Y), X is Y + 1.
% i2(X) :- i5(Y), X is Y + 4. % !! + 4
%
% i4(X) :- i5(Y), X is Y + 1. % !! TABLE table7d
%
% i3(X) :- between(0,1,X). % !! TABLE table7c
%
% i5(Y) :- between(2,3,X).

expected_variants([i1(_),i2(_),i3(_),i4(_),i5(_)]).
expected_answers_for_variant(i1(_),[i1(5),i1(6),i1(7),i1(8),i1(1),i1(2)]).
expected_answers_for_variant(i2(_),[i2(4),i2(5),i2(6),i2(7)]).
expected_answers_for_variant(i3(_),[i3(0),i3(1)]).
expected_answers_for_variant(i4(_),[i4(3),i4(4)]).
expected_answers_for_variant(i5(_),[i5(2),i5(3)]).

i1_expected_answers([5,6,7,8,1,2]).
i2_expected_answers([4,5,6,7]).
i3_expected_answers([0,1]).
i4_expected_answers([3,4]).
i5_expected_answers([2,3]).

:- table i1/1, i2/1, i3/1, i4/1, i5/1.

i1(X) :-
	feedback('i1_aux 1: before'), i2(Y),
	feedback('i1_aux 1: after'),  X is Y + 1.
i1(X) :-
	feedback('i1_aux 2: before'), i3(Y),
	feedback('i1_aux 2: after'),  X is Y + 1.
i2(X) :-
	feedback('i2_aux 1: before'), i4(Y),
	feedback('i2_aux 1: after'),  X is Y + 1.
i2(X) :-
	feedback('i2_aux 2: before'), i5(Y),
	feedback('i2_aux 2: after'),  X is Y + 4.
i4(X) :-
	feedback('i4_aux 1: before'), i5(Y),
	feedback('i4_aux 1: after'),  X is Y + 1.
i3(X) :- between(0,1,X).
i5(X) :- between(2,3,X).

test(ex7) :-
	compare_real_expected_answers(i1,1,i1_expected_answers),
	compare_real_expected_answers(i2,1,i2_expected_answers),
	compare_real_expected_answers(i3,1,i3_expected_answers),
	compare_real_expected_answers(i4,1,i4_expected_answers),
	compare_real_expected_answers(i5,1,i5_expected_answers),
	test_expected_variants_present,
	test_answers_expected_tables.

:- end_tests(tabling_ex7).


		 /*******************************
		 *           EXAMPLE 8		*
		 *******************************/

:- begin_tests(tabling_ex8, [cleanup(abolish_all_tables)]).
:- end_tests(tabling_ex8).


		 /*******************************
		 *	      COMMON		*
		 *******************************/

before    :- debug(tabling, 'before',  []).
before(I) :- debug(tabling, 'before ~w',  [I]).
between   :- debug(tabling, 'between', []).
feedback(Fmt) :- debug(tabling, Fmt, []).
feedback(Fmt,Args) :- debug(tabling, Fmt, Args).
end       :- debug(tabling, 'end',   []).
end(I)    :- debug(tabling, 'end ~w',   [I]).
