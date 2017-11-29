# sym-while, Symbolic Execution for an IMP-like Language

## Building

The build has three dependencies:

1. You must have [OPAM](https://opam.ocaml.org/)
2. You must have `ocamlfind` and `ocamlbuild`:

    % opam install ocamlfind
    % opam install ocamlbuild

3. You must have [Z3](https://github.com/Z3Prover/z3) _with the OCaml bindings_.
    The OCaml bindings can [only be installed from a source build](https://github.com/Z3Prover/z3/issues/1329).

Make sure you have done (1) and (2) before you do (3). The source build will detect if you have OPAM/ocamlfind
and install Z3 as an ocamlfind package if you do.
See: [https://github.com/Z3Prover/z3/tree/master/src/api/ml](https://github.com/Z3Prover/z3/tree/master/src/api/ml).

A note: follow carefully the instructions for building Z3. You must pass a specific flag (--ml) to the Python script
in order to have the build generate and install the OCaml bindings.

After you have the dependencies, you should just be able to do:

    % make

to build the executable.

This has only been tested on OCaml 4.04, so to be safe I recommend:

    % opam switch 4.04

before you resolve any of the dependencies.

## Examples

The res/ directory has a number of examples. You can run them concretely:

    % ./symwhile.native --semantics concrete res/<example_program>

or symbolically:

    % ./symwhile.native --semantics symbolic res/<example_program>
    
