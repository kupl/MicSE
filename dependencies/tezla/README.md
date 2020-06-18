# Tezla

An intermediate representation of Michelson smart contracts designed to ease
static analysis of smart contracts.

## Requirements

You need to install the [Michelson parser and ADT](https://gitlab.com/releaselab/fresco/michelson).

## Install instructions

### Using dune

```bash
git clone https://gitlab.com/releaselab/fresco/tezla.git
cd tezla
dune build @install
dune install
```

### Using opam
```bash
opam pin add tezla https://gitlab.com/releaselab/fresco/tezla.git
```


---

Developed under the [FRESCO](https://release.di.ubi.pt/projects/fresco.html) project
(Formal Verification of Smart Contracts), generously funded by [Tezos
Foundation](https://tezos.foundation).
