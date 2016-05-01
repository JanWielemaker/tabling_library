:- module(gensymlib,
	  [ gensym/2
	  ]).

% Gensym is a library for generating unique symbols (atoms). Such symbols are generated from a base atom which gets a sequence number appended. Of course there is no guarantee that 'catch22' is not an already defined atom and therefore one must be aware that these atoms are only unique in an isolated context.
%
% Description copied from http://www.swi-prolog.org/pldoc/man?section=gensym
%
% Naive implementation for hProlog by Benoit Desouter
%  * this implementation is NOT thread-safe.
%  * there should not be any global variable named gensym_value (or things will get mixed up)

% Initialisation.
:- nb_setval(gensym_value,1).

gensym(Base,Unique) :-
  nb_getval(gensym_value,Counter),
  NewCounter is Counter + 1,
  nb_setval(gensym_value,NewCounter),
  atom_concat(Base,Counter,Unique).
