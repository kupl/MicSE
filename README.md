# MicSE (in development)

## Table of Contents

- [MicSE (in development)](#micse-in-development)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Documents](#documents)
  - [Development](#development)
    - [Prerequisite](#prerequisite)
      - [System packages](#system-packages)
      - [Opam packages](#opam-packages)
    - [Git Commit Guidelines](#git-commit-guidelines)
  - [About](#about)

## Introduction

MicSE is a tool for verifying and testing Tezos smart contracts written in Michelson. Michelson is a native smart contract language of the [Tezos Blockchain](https://tezos.foundation).

MicSE can verify the absence of runtime failures of Michelson programs, e.g., no arithmetic overflow. Also, MicSE supports verification of user-provided assertions. 

- *Prover* (=verifier)
- *Refuter* (=tester, will be provided)

## Documents

- [Installation](./doc/INSTALLATION.md)
- [Usage](./doc/USAGE.md)

## Development

### Prerequisite

#### System packages

- OCaml (v.4.10.0): We use OCaml language to programming.
- Opam (v2.0.4): We use Opam to manage package of OCaml.

#### Opam packages

- Batteries
- Core (v0.14.0)
- Dune (v2.4.0): We use Dune to manage build system of OCaml project.
- Logs
- Menhir
- Mtime
- Ptime
- Ocamlgraph
- Yojson
- Z3 (v4.8.9)
- Zarith

### Git Commit Guidelines

We are following [Angular's commitizen rules](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#-git-commit-guidelines) for formatting git commit message. This allows you to read messages that are easy to understand when looking for project history. It also uses the git commit message to generate our [CHANGELOG](/CHANGELOG.md) file.

## About

This project is funded by Tezos Foundation: [Verification and Testing Infrastructure for Tezos Contracts](https://tezos.foundation/fourth-cohort-grants/).

Authored and maintained by **Jisuk Byun**, **Heewoong Jang**

> Github [@cushionbadak](https://github.com/cushionbadak)  
> Github [@JormalHeewoongJang](https://github.com/jormal)
