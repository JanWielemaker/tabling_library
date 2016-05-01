:- module(variantlib,
	  [ variant/2
	  ]).
% FIXME
:- ensure_loaded(['testlib.pl']).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
auto_test(t_variant_alt).
% Test for variant_alt/2:
% Taken from the SWI-Prolog description of (=@=) at http://www.swi-prolog.org/pldoc/doc_for?object=(%3D@%3D)/2
t_variant_alt :-
  expect_fail(variant_alt(a,_A1)),
  expect_true(variant_alt(_A2,_B2)),
  expect_fail(variant_alt(x(A3,A3),x(_B3,_C3))),
  expect_true(variant_alt(x(A4,A4),x(B4,B4))),
  expect_fail(variant_alt(x(A5,A5),x(A5,_B5))),
  expect_true(variant_alt(x(_A6,_B6),x(_C6,_D6))),
  expect_true(variant_alt(x(A7,B7),x(B7,A7))),
  expect_true(variant_alt(x(A8,_B8),x(_C8,A8))),
  % One extra, which made us aware of the existence of two alternatives for variant/2.
  expect_true(variant_alt(x(A9,_B9),x(A9,_C9))).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% Variant/2
% Does not behave as I (personally) would expect on p(X,Y) and p(X,Z).
% In SWI-Prolog:
%   benoit we02c131 tabling$ swipl
%   ...
%   ?- variant(p(X,Y),p(X,Z)).
%   true.
% This version in hProlog:
%   ?- variant(p(X,Y),p(X,Z)).
%   No more!
%   The cause seems to be numbervars:
%   ?- numbervars(p(X,Y),0,N),numbervars(p(X,Z),0,M).
%   M = 1
%   Z = X = A
%   N = 2
%   Y = B
% For our purpose we expected M to be 2.
% ?- variant(X,X).
%  No more !
%
% ?- variant(f(a,_),f(a,_)).
%
% Yes
% ?- variant(f(a,X),f(a,X)).
% No more !
variant(T1,T2) :-
  \+(\+(v(T1,T2))).

% Alternative that has the SWI-behaviour.
% The right implementation for variant/2 has been a topic of discussion in the community.
% Thanks to Bart for making me aware of this alternative.
variant_alt(T1,T2) :-
  copy_term(T2,T3),
  \+(\+(v(T1,T3))).

v(T1,T2) :-
  numbervars(T1,0,N),
  numbervars(T2,0,N),
  T1 = T2.
