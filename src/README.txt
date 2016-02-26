Instructions for building


The Makefile depends on SLAyer/config.sh having been sourced:
    $ source ../config.sh


- To compile debug (byte and native) code:
    $ make dbg

- To compile optimized (native) code:
    $ make opt

    Executables are dropped in ../bin

Warning: The dependency tracking for the OCaml Z3 bindings is not
robust enough to recover from failed builds, so if the make falls over
while trying to build z3dll, execute 'make clean_mlz3' before retrying
'make'.

- To compile only a single module (and its dependencies):
    $ make M=<module> module
