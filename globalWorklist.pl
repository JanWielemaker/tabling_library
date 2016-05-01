:- module(global_worklist,
	  [ add_to_global_worklist/1,
	    worklist_empty/0,
	    pop_worklist/1
	  ]).

add_to_global_worklist(TableIdentifier) :-
  nb_getval(globalWorklist,L1),
  nb_linkval(globalWorklist,[TableIdentifier|L1]).

worklist_empty :-
  nb_getval(globalWorklist,[]).

pop_worklist(TableIdentifier) :-
  nb_getval(globalWorklist,L1),
  L1 = [TableIdentifier|L2],
  nb_linkval(globalWorklist,L2).
