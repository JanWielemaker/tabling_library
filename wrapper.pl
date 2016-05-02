:- module(table_wrapper,
	  [ (table)/1,			% +Predicates

	    op(1150, fx, table)
	  ]).
:- use_module(library(error)).

:- multifile
	user:term_expansion/2,
	tabled/2.
:- dynamic
	user:term_expansion/2.

%%	table(+PredicateIndicators)
%
%	Prepare the given PredicateIndicators for tabling.  Can only
%	be used as a directive.

table(PredicateIndicators) :-
	throw(error(context_error(nodirective, table(PredicateIndicators)), _)).


wrappers(Var) -->
	{ var(Var), !,
	  instantiation_error(Var)
	}.
wrappers((A,B)) --> !,
	wrappers(A),
	wrappers(B).
wrappers(Name//Arity) -->
	{ atom(Name), integer(Arity), Arity >= 0, !,
	  Arity1 is Arity+2
	},
	wrappers(Name/Arity1).
wrappers(Name/Arity) -->
	{ atom(Name), integer(Arity), Arity >= 0, !,
	  functor(Head, Name, Arity),
	  atom_concat(Name, ' tabled', WrapName),
	  Head =.. [Name|Args],
	  WrappedHead =.. [WrapName|Args],
	  prolog_load_context(module, Module)
	},
	[ table_wrapper:tabled(Head, Module),
	  (   Head :-
		 start_tabling(Head, WrappedHead)
	  )
	].

rename((Head :- Body), (NewHead :- Body)) :- !,
	rename(Head, NewHead).
rename((Head --> Body), (NewHead --> Body)) :- !,
	prolog_load_context(module, Module),
	functor(Head, Name, Arity),
	PlainArity is Arity+1,
	functor(PlainHead, Name, PlainArity),
	tabled(PlainHead, Module),
	rename_term(Head, NewHead).
rename(Head, NewHead) :-
	prolog_load_context(module, Module),
	tabled(Head, Module), !,
	rename_term(Head, NewHead).

rename_term(Compound0, Compound) :-
	compound(Compound0), !,
	compound_name_arguments(Compound0, Name, Args),
	atom_concat(Name, ' tabled', WrapName),
	compound_name_arguments(Compound, WrapName, Args).
rename_term(Name, WrapName) :-
	atom_concat(Name, ' tabled', WrapName).


user:term_expansion((:- table(Preds)),
		    [ (:- multifile table_wrapper:tabled/2)
		    | Clauses
		    ]) :-
	phrase(wrappers(Preds), Clauses).
user:term_expansion(Clause, NewClause) :-
	rename(Clause, NewClause).
