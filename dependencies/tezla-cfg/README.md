# Tezla CFG generator

A Control-flow graph generator for the Tezla representation.

## Requirements

* [Michelson parser and ADT](https://gitlab.com/releaselab/fresco/michelson)
* [Tezla](https://gitlab.com/releaselab/fresco/tezla)

## Install instructions

### Using dune

```bash
git clone https://gitlab.com/releaselab/fresco/tezla-cfg.git
cd tezla-cfg
dune build @install
dune install
```

### Using opam
```bash
opam pin add tezla-cfg https://gitlab.com/releaselab/fresco/tezla-cfg.git
```


---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html) project
(Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
