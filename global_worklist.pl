:- module(global_worklist,
	  [ add_to_global_worklist/1,
	    worklist_empty/0,
	    empty_worklist/0,
	    pop_worklist/1
	  ]).

add_to_global_worklist(TableIdentifier) :-
  nb_getval(table_global_worklist,L1),
  nb_linkval(table_global_worklist,[TableIdentifier|L1]).

worklist_empty :-
  nb_getval(table_global_worklist,[]).

pop_worklist(TableIdentifier) :-
  nb_getval(table_global_worklist,[TableIdentifier|L2]),
  nb_linkval(table_global_worklist,L2).

empty_worklist :-
  nb_linkval(table_global_worklist, []).
