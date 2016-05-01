add_to_global_worklist(TableIdentifier) :-
  nb_getval(globalWorklist,L1),
  nb_setval(globalWorklist,[TableIdentifier|L1]).

worklist_empty :-
  nb_getval(globalWorklist,[]).

pop_worklist(TableIdentifier) :-
  nb_getval(globalWorklist,L1),
  L1 = [TableIdentifier|L2],
  nb_setval(globalWorklist,L2).
