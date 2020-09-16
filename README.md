# MicSE (in development)

## Table of Contents

- [MicSE](#micse)
  - [Table of Contents](#table-of-contents)
  - [Introduction](#introduction)
  - [Development](#development)
    - [Prerequisite](#prerequisite)
    - [Git Commit Guidelines](#git-commit-guidelines)
  - [About](#about)

## Introduction

MicSE, Michelson Symbolic vErifier, is the tool for verifying and testing the integrity of smart contracts written in Michelson. Michelson is the smart-contract-programming language used in [Tezos Blockchain](https://tezos.foundation).

MicSE provides two tools, Prover and Tester for Michelson.
- *Prover* (=verifier)
- *Refuter* (=tester)

## Development

### Prerequisite

#### System packages
- OCaml (v.4.10.0): We use OCaml language to programming.
- Opam (v2.0.4): We use Opam to manage package of OCaml.

#### Opam packages
- Batteries
- Core (v0.14.0)
- Dune (v2.2.0): We use Dune to manage build system of OCaml project.
- Menhir
- Ocamlgraph
- Z3
- Zarith

### Git Commit Guidelines

We are following [Angular's commitizen rules](https://github.com/angular/angular.js/blob/master/DEVELOPERS.md#-git-commit-guidelines) for formatting git commit message. This allows you to read messages that are easy to understand when looking for project history. It also uses the git commit message to generate our [CHANGELOG](/CHANGELOG.md) file.

## About

Authored and maintained by **Jisuk Byun**, **Heewoong Jang**

> Github [@cushionbadak](https://github.com/cushionbadak)  
> Github [@JormalHeewoongJang](https://github.com/jormal)
