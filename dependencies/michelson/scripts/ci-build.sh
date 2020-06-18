#!/bin/bash
opam switch $1
eval $(opam config env) 
opam pin add -n .
opam depext michelson
opam install . --deps-only
dune build @install
