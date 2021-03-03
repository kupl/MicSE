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

```json
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
