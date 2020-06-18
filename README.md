# MicSE

## Table of Contents

- [MicSE](#micse)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Development](#development)
    - [Dependencies](#dependencies)
    - [Prerequisite](#prerequisite)
    - [Git Commit Guidelines](#git-commit-guidelines)
  - [About](#about)

## Introduction

Michelson Symbolic vErifier, for verifying and testing the integrity of smart contracts in the Tezos blockchain.

## Development

### Dependencies

- Michelson: Michelson ADT parser from FRESCO.
- Tezla: Tezla project from FRESCO.
- Tezla-cfg: Tezla-cfg project from FRESCO.

``` bash
$ opam pin add michelson dependencies/michelson
[michelson.~dev] synchronised from file://...
michelson is now pinned to file://...
...

$ opam pin add tezla dependencies/tezla
[tezla.~dev] synchronised from file://...
tezla is now pinned to file://...
...

$ opam pin add tezla-cfg dependencies/tezla-cfg
[tezla-cfg.~dev] synchronised from file://...
tezla-cfg is now pinned to file://...
...
```

### Prerequisite

- OCaml (v.4.10.0): We use OCaml language to programming.
- Opam (v2.0.4): We use Opam to manage package of OCaml.
- Dune (v2.2.0): We use Dune to manage build system of OCaml project.

### Git Commit Guidelines

We are following [Angular's commitizen rules](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#-git-commit-guidelines) for formatting git commit message. This allows you to read messages that are easy to understand when looking for project history. It also uses the git commit message to generate our [CHANGELOG](/CHANGELOG.md) file.

## About

Authored and maintained by **Jisuk Byun**, **Heewoong Jang**

> Github [@cushionbadak](https://github.com/cushionbadak)  
> Github [@JormalHeewoongJang](https://github.com/jormal)
