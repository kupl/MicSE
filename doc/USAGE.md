# Usage of MicSE

## Table of Contents

- [Usage of MicSE](#usage-of-micse)
  - [Table of Contents](#table-of-contents)
  - [Build](#build)
    - [Install build packages](#install-build-packages)
    - [Clone, Build](#clone-build)
  - [Run](#run)
    - [MicSE Prover](#micse-prover)
      - [Options](#options)
      - [JSON Output Format](#json-output-format)
      - [Example](#example)

## Build

### Install build packages

MicSE uses these packages.

| System Package Name | Version |
| :------------------ | :-----: |
| make                |  4.2.1  |
| ocaml               | 4.10.0  |
| opam                |  2.0.4  |

| Opam Package Name | Version |
| :---------------- | :-----: |
| Batteries         |    -    |
| Core              | 0.14.0  |
| Dune              |  2.4.0  |
| Menhir            |    -    |
| Ptime             |    -    |
| Ocamlgraph        |    -    |
| Yojson            |    -    |
| Z3                |  4.8.9  |
| Zarith            |    -    |

### Clone, Build

We are not providing the version build file now.
To use the tool of MicSE, you have to clone this repository and build it manually.

```bash
$ git clone https://github.com/kupl/MicSE.git
$ cd MicSE
$ make
dune build
...
```

## Run

Binary execution files are located at `(PROJECT_DIR)/bin` directory.

### MicSE Prover

```bash
$ (PROJECT_DIR)/bin/micse_prover --input (FILE_PATH)
# of Instructions : _
# of Total Queries : _
# of Proved Queries : _
# of Unproved Queries : _
================ Proved Queries ::
...
================ Unproved Queries ::
...
```

- **Input:**
  - Michelson program code
  - (Optional) User custom safety property
  - (Optional) Initial storage data
- **Output:**
  - Prove result of each safety property
  - (Optional) CFG of code

#### Options

- `--input (FILE_PATH)`: File path for input michelson program code.
- `--json-out (FILE_PATH)`: File path for JSON output file.
- `--initial-storage (FILE_PATH)`: File path for initial storage of input michelson program.
- `--z3-timeout`: Time budget for z3 solver in seconds. (Default: 30s)
- `--prover-timeout`: Time budget for prover in seconds. (Default: 180s)

#### JSON Output Format

```txt
{
  "file": "", // Input File Path
  "instructions": 0, // Number of Instructions
  "result": {
    "prover": {
      "total": 0, // Number of Total Queries
      "proved": 0, // Number of Proved Queries
      "unproved": 0, // Number of Unproved Queries
      "queries": [
        {
          "id": 0, // ID of Query
          "state": "Proven" | "Unproven", // Proved State of Query at the Result
          "vertex": 0, // ID of Vertex on CFG
          "position": "0:0-0:0", // Location of Query in code "(start_line):(start_column)-(end_line):(end_column)"
          "category": "Q_mutez_arith_safety" | "Q_int_nat_shift_safety" | "Q_assertion", // Property Category of Query
        }, ... ]
    }
  },
  "cfg": {
    "edge": [ "0 -> 0 ;", ... ], // Edge of CFG "(vertex) -> (vertex) ;"
    "vertex": [ "0 [label=\"0 : v0 := (EXPRRESION)\", ...] ;", ... ] // Vertex of CFG "(vertex) [label, shape, style]"
  }
}
```

#### Example

- **Input:** [`transfer_safe.tz`](../benchmarks/toys/transfer_safe.tz)
- **Output:**

```txt
$ ./bin/micse_prover --input ./benchmarks/toys/add1_guarded.tz --json-out add1_guarded.json
# of Instructions : 14
# of Total Queries : 1
# of Proved Queries : 1
# of Unproved Queries : 0
================ Proved Queries ::
======== Proved Query #1, vtx=44, pos=9:17-9:20, category="Q_mutez_arith_safety"
==== Transaction Invariant (printed in optimized form):
True
==== Loop Invariant (printed in optimized form):
================ Unproved Queries ::
```

```json
{
  "file": "./benchmarks/toys/add1_guarded.tz",
  "instructions": 14,
  "result": {
    "prover": {
      "total": 1,
      "proved": 1,
      "unproved": 0,
      "queries": [
        {
          "id": 1,
          "state": "Proven",
          "vertex": 44,
          "position": "9:17-9:20",
          "category": "Q_mutez_arith_safety",
          "invariant": { "transaction": "True", "loop": [] }
        }
      ]
    }
  },
  "cfg": {
    "edge": [
      "48 -> 1 ;", "47 -> 46 ;", "46 -> 48 ;", "45 -> 47 ;", "44 -> 43 ;",
      "43 -> 45 ;", "42 -> 44 ;", "41 -> 40 ;", "40 -> 42 ;", "39 -> 41 ;",
      "38 -> 27 [label=\"Failed\", style=dotted] ;", "37 -> 36 ;",
      "36 -> 38 ;", "35 -> 37 ;", "34 -> 29 ;", "33 -> 32 ;", "32 -> 34 ;",
      "31 -> 33 ;", "30 -> 31 ;", "29 -> 26 ;", "28 -> 35 ;",
      "27 -> 26 [label=\"Failed\", style=dotted] ;", "26 -> 39 ;",
      "25 -> 30 [label=\"True\"] ;", "25 -> 28 [label=\"False\"] ;",
      "25 -> 26 [label=\"If_skip\", style=dotted] ;", "24 -> 23 ;",
      "23 -> 25 ;", "22 -> 24 ;", "21 -> 20 ;", "20 -> 22 ;", "19 -> 21 ;",
      "18 -> 17 ;", "17 -> 19 ;", "16 -> 18 ;", "15 -> 14 ;", "14 -> 16 ;",
      "13 -> 15 ;", "12 -> 11 ;", "11 -> 3 ;", "10 -> 12 ;", "9 -> 8 ;",
      "8 -> 10 ;", "7 -> 9 ;", "6 -> 5 ;", "5 -> 7 ;", "4 -> 6 ;",
      "3 -> 13 ;", "2 -> 4 ;", "0 -> 2 ;"
    ],
    "vertex": [
      "48 [label=\"48 : v11 := PAIR v10 v9\", shape=box] ;",
      "47 [label=\"47 : v10 := NIL operation\", shape=box] ;",
      "46 [label=\"46 : skip\"] ;", "45 [label=\"45 : skip\"] ;",
      "44 [label=\"44 : v9 := ADD v8 v1\", shape=box] ;",
      "43 [label=\"43 : skip\"] ;", "42 [label=\"42 : skip\"] ;",
      "41 [label=\"41 : v8 := PUSH 1 mutez\", shape=box] ;",
      "40 [label=\"40 : skip\"] ;", "39 [label=\"39 : skip\"] ;",
      "38 [label=\"38 : FAILWITH v7\", shape=cds] ;",
      "37 [label=\"37 : v7 := PUSH \\\"param < 9223372036854775807\\\" string\", shape=box] ;",
      "36 [label=\"36 : skip\"] ;", "35 [label=\"35 : skip\"] ;",
      "34 [label=\"34 : DROP [v2]\"] ;", "33 [label=\"33 : SWAP\"] ;",
      "32 [label=\"32 : skip\"] ;", "31 [label=\"31 : skip\"] ;",
      "30 [label=\"30 : skip\"] ;", "29 [label=\"29 : skip\"] ;",
      "28 [label=\"28 : skip\"] ;",
      "27 [label=\"27 : FAILWITH v7\", shape=cds] ;",
      "26 [label=\"26 : skip\"] ;",
      "25 [label=\"25 : IF v6\", shape=diamond] ;",
      "24 [label=\"24 : v6 := GT v5\", shape=box] ;",
      "23 [label=\"23 : skip\"] ;", "22 [label=\"22 : skip\"] ;",
      "21 [label=\"21 : v5 := COMPARE v4 v1\", shape=box] ;",
      "20 [label=\"20 : skip\"] ;", "19 [label=\"19 : skip\"] ;",
      "18 [label=\"18 : v4 := PUSH 9223372036854775807 mutez\", shape=box] ;",
      "17 [label=\"17 : skip\"] ;", "16 [label=\"16 : skip\"] ;",
      "15 [label=\"15 : v3 := DUP v1\", shape=box] ;",
      "14 [label=\"14 : skip\"] ;", "13 [label=\"13 : skip\"] ;",
      "12 [label=\"12 : v2 := CDR param_storage\", shape=box] ;",
      "11 [label=\"11 : skip\"] ;", "10 [label=\"10 : skip\"] ;",
      "9 [label=\"9 : v1 := CAR param_storage\", shape=box] ;",
      "8 [label=\"8 : skip\"] ;", "7 [label=\"7 : skip\"] ;",
      "6 [label=\"6 : v0 := DUP param_storage\", shape=box] ;",
      "5 [label=\"5 : skip\"] ;", "4 [label=\"4 : skip\"] ;",
      "3 [label=\"3 : skip\"] ;", "2 [label=\"2 : skip\"] ;",
      "1 [label=\"1 : MAIN-EXIT : v11\", shape=doubleoctagon] ;",
      "0 [label=\"0 : MAIN-ENTRY\", shape=doubleoctagon] ;"
    ]
  }
}
```
