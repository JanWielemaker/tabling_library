Tabling as a Library with Delimited Control
===========================================

This library is described in the paper "Tabling as Library with Delimited
Control" by Benoit Desouter, Marko van Dooren and Tom Schrijvers.

Email: Benoit dot Desouter at UGent dot be

## SWI-Prolog port

### Usage

  - Include `:- use_module('/path/to/tabling_library/tabling').`
  - Use :- table name/arity, ... .

### Changes for this port

This repository contains the port to SWI-Prolog.  Summary of changes:

  - Added modules to files
  - Used SWI-Prolog libraries (assoc, gensym)
  - Some nb_link{arg,val} changes.  Use duplicate_term/2 rather
    than copy_term/2.
  - Avoid using common names for global variables
  - Initialize global variables lazily, so it works in any thread.
  - Added automatic wrapper generation
  - Turned examples into test_tabling.pl and added `make check`
  - Added XSB abolish_all_tables/0

Requires GIT version (https://github.com/SWI-Prolog/swipl-devel.pl)

### Status

Pretty experimental.

  - Using nb_{set,link}{arg,val} to manage the tables on the stack is
  fragile. At this moment, **tabling goes wrong if the debugger is
  enabled**.
  - Exceptions while solving a tabled predicate leaves incomplete
  tables.  Use `?- abolish_all_tables.` before continuing.


### Plans

  - Move table/trie store to C for performance and to get rid of
  the ill defined behaviour on backtracking.
  - Deal with exceptions
  - Add more table management predicates from XSB.
  - Much more
