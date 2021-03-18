# Usage of MicSE

## Table of Contents

- [Usage of MicSE](#usage-of-micse)
  - [Table of Contents](#table-of-contents)
  - [Run](#run)
    - [MicSE Prover](#micse-prover)
      - [Options](#options)
      - [JSON Output Format](#json-output-format)
      - [Example](#example)
  - [User Custom Safety Property](#user-custom-safety-property)
    - [Example](#example-1)

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
  - (Optional) [User custom safety property](#user-custom-safety-property)
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
          "invariant": {
            "transaction": "..." // Formula which used as a transaction invariant
            "loop": [ "...", ... ] // Formulas which used as a loop invariant
          }
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

- **Demo Video:** [Link](https://youtu.be/9itXEQY051Y)
- **Input:** [`transfer_safe.tz`](../benchmarks/toys/transfer_safe.tz)
- **Output:**

```txt
$ ./bin/micse_prover --input ./benchmark/toys/transfer_safe.tz --z3-timeout 600 --prover-timeout 1800 --json-out ./transfer_safe.json
# of Instructions : 48
# of Total Queries : 3
# of Proved Queries : 3
# of Unproved Queries : 0
================ Proved Queries ::
======== Proved Query #1, vtx=109, pos=10:25-10:28, category="Q_mutez_arith_safety"
==== Transaction Invariant (printed in optimized form):
True
==== Loop Invariant (printed in optimized form):
======== Proved Query #2, vtx=150, pos=14:18-14:21, category="Q_mutez_arith_safety"
==== Transaction Invariant (printed in optimized form):
MtzMap_PSE(CAR(-trxStorage-0),(v25,v10),CDR(-trxStorage-0),__MTZMAP_SUM_R_(CAR(-trxStorage-0))_(v25)_(v10)) /\ NoUnderflow_SUB(CDR(-trxStorage-0),CDR(-trxStorage-0))
==== Loop Invariant (printed in optimized form):
======== Proved Query #3, vtx=155, pos=16:4-16:52, category="Q_assertion"
==== Transaction Invariant (printed in optimized form):
MtzMap_PSE(CAR(-trxStorage-0),(v25,v10),CDR(-trxStorage-0),__MTZMAP_SUM_R_(CAR(-trxStorage-0))_(v25)_(v10)) /\ NoUnderflow_SUB(CDR(-trxStorage-0),CDR(-trxStorage-0))
==== Loop Invariant (printed in optimized form):
================ Unproved Queries ::
```

```json
{
  "file": "./benchmarks/toys/transfer_safe.tz",
  "instructions": 48,
  "result": {
    "prover": {
      "total": 3,
      "proved": 3,
      "unproved": 0,
      "queries": [
        {
          "id": 1,
          "state": "Proven",
          "vertex": 109,
          "position": "10:25-10:28",
          "category": "Q_mutez_arith_safety",
          "invariant": { "transaction": "True", "loop": [] }
        },
        {
          "id": 2,
          "state": "Proven",
          "vertex": 150,
          "position": "14:18-14:21",
          "category": "Q_mutez_arith_safety",
          "invariant": {
            "transaction":
              "MtzMap_PSE(CAR(-trxStorage-0),(v25,v10),CDR(-trxStorage-0),__MTZMAP_SUM_R_(CAR(-trxStorage-0))_(v25)_(v10)) /\\ NoUnderflow_SUB(CDR(-trxStorage-0),CDR(-trxStorage-0))",
            "loop": []
          }
        },
        {
          "id": 3,
          "state": "Proven",
          "vertex": 155,
          "position": "16:4-16:52",
          "category": "Q_assertion",
          "invariant": {
            "transaction":
              "MtzMap_PSE(CAR(-trxStorage-0),(v25,v10),CDR(-trxStorage-0),__MTZMAP_SUM_R_(CAR(-trxStorage-0))_(v25)_(v10)) /\\ NoUnderflow_SUB(CDR(-trxStorage-0),CDR(-trxStorage-0))",
            "loop": []
          }
        }
      ]
    }
  },
  "cfg": {
    "edge": [
      "187 -> 1 ;", "186 -> 185 ;", "185 -> 187 ;", "184 -> 186 ;",
      "183 -> 182 ;", "182 -> 184 ;", "181 -> 183 ;", "180 -> 179 ;",
      "179 -> 181 ;", "178 -> 180 ;", "177 -> 176 ;", "176 -> 178 ;",
      "175 -> 177 ;", "174 -> 173 ;", "173 -> 175 ;", "172 -> 174 ;",
      "171 -> 155 ;", "170 -> 169 ;", "169 -> 171 ;", "168 -> 170 ;",
      "167 -> 163 ;", "166 -> 165 ;", "165 -> 167 ;", "164 -> 166 ;",
      "163 -> 161 ;", "162 -> 164 ;", "161 -> 168 ;", "160 -> 162 ;",
      "159 -> 154 ;", "158 -> 159 ;", "157 -> 158 ;", "156 -> 157 ;",
      "155 -> 152 ;", "154 -> 160 ;", "153 -> 156 ;",
      "153 -> 152 [label=\"Check_skip\", style=dotted] ;", "152 -> 172 ;",
      "151 -> 153 ;", "150 -> 149 ;", "149 -> 151 ;", "148 -> 150 ;",
      "147 -> 146 ;", "146 -> 144 ;", "145 -> 147 ;", "144 -> 148 ;",
      "143 -> 145 ;", "142 -> 134 ;", "141 -> 134 ;", "140 -> 135 ;",
      "139 -> 137 ;", "138 -> 139 ;", "137 -> 142 ;", "136 -> 140 ;",
      "135 -> 141 ;", "134 -> 143 ;", "133 -> 138 [label=\"True\"] ;",
      "133 -> 136 [label=\"False\"] ;",
      "133 -> 134 [label=\"If_skip\", style=dotted] ;", "132 -> 131 ;",
      "131 -> 133 ;", "130 -> 132 ;", "129 -> 123 ;", "128 -> 127 ;",
      "127 -> 125 ;", "126 -> 128 ;", "125 -> 129 ;", "124 -> 126 ;",
      "123 -> 130 ;", "122 -> 124 ;", "121 -> 120 ;", "120 -> 122 ;",
      "119 -> 121 ;", "118 -> 117 ;", "117 -> 119 ;", "116 -> 118 ;",
      "115 -> 114 ;", "114 -> 116 ;", "113 -> 115 ;", "112 -> 111 ;",
      "111 -> 113 ;", "110 -> 112 ;", "109 -> 108 ;", "108 -> 110 ;",
      "107 -> 109 ;", "106 -> 97 ;", "105 -> 104 ;", "104 -> 102 ;",
      "103 -> 105 ;", "102 -> 106 ;", "101 -> 103 ;", "100 -> 99 ;",
      "99 -> 101 ;", "98 -> 100 ;", "97 -> 95 ;", "96 -> 98 ;",
      "95 -> 107 ;", "94 -> 96 ;",
      "93 -> 86 [label=\"Failed\", style=dotted] ;", "92 -> 91 ;",
      "91 -> 93 ;", "90 -> 92 ;", "89 -> 88 ;", "88 -> 85 ;", "87 -> 90 ;",
      "86 -> 85 [label=\"Failed\", style=dotted] ;", "85 -> 94 ;",
      "84 -> 89 [label=\"True\"] ;", "84 -> 87 [label=\"False\"] ;",
      "84 -> 85 [label=\"If_skip\", style=dotted] ;", "83 -> 82 ;",
      "82 -> 84 ;", "81 -> 83 ;", "80 -> 79 ;", "79 -> 81 ;", "78 -> 80 ;",
      "77 -> 71 ;", "76 -> 75 ;", "75 -> 73 ;", "74 -> 76 ;", "73 -> 77 ;",
      "72 -> 74 ;", "71 -> 78 ;", "70 -> 72 ;", "69 -> 63 ;", "68 -> 67 ;",
      "67 -> 65 ;", "66 -> 68 ;", "65 -> 69 ;", "64 -> 66 ;", "63 -> 70 ;",
      "62 -> 64 ;", "61 -> 53 ;",
      "60 -> 55 [label=\"Failed\", style=dotted] ;", "59 -> 58 ;",
      "58 -> 60 ;", "57 -> 59 ;", "56 -> 57 ;",
      "55 -> 52 [label=\"Failed\", style=dotted] ;", "54 -> 61 ;",
      "53 -> 52 ;", "52 -> 62 ;", "51 -> 56 [label=\"True\"] ;",
      "51 -> 54 [label=\"False\"] ;",
      "51 -> 52 [label=\"If_skip\", style=dotted] ;", "50 -> 49 ;",
      "49 -> 51 ;", "48 -> 50 ;", "47 -> 46 ;", "46 -> 48 ;", "45 -> 47 ;",
      "44 -> 38 ;", "43 -> 42 ;", "42 -> 40 ;", "41 -> 43 ;", "40 -> 44 ;",
      "39 -> 41 ;", "38 -> 45 ;", "37 -> 39 ;", "36 -> 35 ;", "35 -> 27 ;",
      "34 -> 36 ;", "33 -> 32 ;", "32 -> 34 ;", "31 -> 33 ;", "30 -> 29 ;",
      "29 -> 31 ;", "28 -> 30 ;", "27 -> 25 ;", "26 -> 28 ;", "25 -> 37 ;",
      "24 -> 26 ;", "23 -> 22 ;", "22 -> 14 ;", "21 -> 23 ;", "20 -> 19 ;",
      "19 -> 21 ;", "18 -> 20 ;", "17 -> 16 ;", "16 -> 18 ;", "15 -> 17 ;",
      "14 -> 24 ;", "13 -> 15 ;", "12 -> 11 ;", "11 -> 3 ;", "10 -> 12 ;",
      "9 -> 8 ;", "8 -> 10 ;", "7 -> 9 ;", "6 -> 5 ;", "5 -> 7 ;",
      "4 -> 6 ;", "3 -> 13 ;", "2 -> 4 ;", "0 -> 2 ;"
    ],
    "vertex": [
      "187 [label=\"187 : v41 := PAIR v40 v39\", shape=box] ;",
      "186 [label=\"186 : v40 := NIL operation\", shape=box] ;",
      "185 [label=\"185 : skip\"] ;", "184 [label=\"184 : skip\"] ;",
      "183 [label=\"183 : v39 := PAIR v38 v8\", shape=box] ;",
      "182 [label=\"182 : skip\"] ;", "181 [label=\"181 : skip\"] ;",
      "180 [label=\"180 : v38 := UPDATE v4 v37 v23\", shape=box] ;",
      "179 [label=\"179 : skip\"] ;", "178 [label=\"178 : skip\"] ;",
      "177 [label=\"177 : DIG\"] ;", "176 [label=\"176 : skip\"] ;",
      "175 [label=\"175 : skip\"] ;",
      "174 [label=\"174 : v37 := SOME v30\", shape=box] ;",
      "173 [label=\"173 : skip\"] ;", "172 [label=\"172 : skip\"] ;",
      "171 [label=\"171 : v36 := LEQ v35\", shape=box] ;",
      "170 [label=\"170 : v35 := COMPARE v31 v34\", shape=box] ;",
      "169 [label=\"169 : skip\"] ;", "168 [label=\"168 : skip\"] ;",
      "167 [label=\"167 : DROP [v33]\"] ;",
      "166 [label=\"166 : DROP [v32]\"] ;", "165 [label=\"165 : skip\"] ;",
      "164 [label=\"164 : skip\"] ;", "163 [label=\"163 : skip\"] ;",
      "162 [label=\"162 : skip\"] ;", "161 [label=\"161 : skip\"] ;",
      "160 [label=\"160 : skip\"] ;",
      "159 [label=\"159 : v34 := v8\", shape=box] ;",
      "158 [label=\"158 : v33 := v4\", shape=box] ;",
      "157 [label=\"157 : v32 := v23\", shape=box] ;",
      "156 [label=\"156 : v31 := v30\", shape=box] ;",
      "155 [label=\"155 : #MICSE_check_value v36\"] ;",
      "154 [label=\"154 : skip\"] ;",
      "153 [label=\"153 : #MICSE_check_entry\"] ;",
      "152 [label=\"152 : skip\"] ;", "151 [label=\"151 : skip\"] ;",
      "150 [label=\"150 : v30 := ADD v29 v5\", shape=box] ;",
      "149 [label=\"149 : skip\"] ;", "148 [label=\"148 : skip\"] ;",
      "147 [label=\"147 : DIG\"] ;", "146 [label=\"146 : skip\"] ;",
      "145 [label=\"145 : skip\"] ;", "144 [label=\"144 : skip\"] ;",
      "143 [label=\"143 : skip\"] ;",
      "142 [label=\"142 : v29 := v27\", shape=box] ;",
      "141 [label=\"141 : v29 := v28\", shape=box] ;",
      "140 [label=\"140 : skip\"] ;",
      "139 [label=\"139 : v27 := PUSH 0 mutez\", shape=box] ;",
      "138 [label=\"138 : skip\"] ;", "137 [label=\"137 : skip\"] ;",
      "136 [label=\"136 : v28 := unlift_option v26\", shape=box] ;",
      "135 [label=\"135 : skip\"] ;", "134 [label=\"134 : skip\"] ;",
      "133 [label=\"133 : IF_NONE v26\", shape=diamond] ;",
      "132 [label=\"132 : v26 := GET v4 v23\", shape=box] ;",
      "131 [label=\"131 : skip\"] ;", "130 [label=\"130 : skip\"] ;",
      "129 [label=\"129 : DIG\"] ;",
      "128 [label=\"128 : v25 := DUP v4\", shape=box] ;",
      "127 [label=\"127 : skip\"] ;", "126 [label=\"126 : skip\"] ;",
      "125 [label=\"125 : skip\"] ;", "124 [label=\"124 : skip\"] ;",
      "123 [label=\"123 : skip\"] ;", "122 [label=\"122 : skip\"] ;",
      "121 [label=\"121 : v24 := DUP v23\", shape=box] ;",
      "120 [label=\"120 : skip\"] ;", "119 [label=\"119 : skip\"] ;",
      "118 [label=\"118 : v23 := UPDATE v22 v21 v7\", shape=box] ;",
      "117 [label=\"117 : skip\"] ;", "116 [label=\"116 : skip\"] ;",
      "115 [label=\"115 : v22 := SENDER\", shape=box] ;",
      "114 [label=\"114 : skip\"] ;", "113 [label=\"113 : skip\"] ;",
      "112 [label=\"112 : v21 := SOME v20\", shape=box] ;",
      "111 [label=\"111 : skip\"] ;", "110 [label=\"110 : skip\"] ;",
      "109 [label=\"109 : v20 := SUB v13 v5\", shape=box] ;",
      "108 [label=\"108 : skip\"] ;", "107 [label=\"107 : skip\"] ;",
      "106 [label=\"106 : DIG\"] ;",
      "105 [label=\"105 : v19 := DUP v5\", shape=box] ;",
      "104 [label=\"104 : skip\"] ;", "103 [label=\"103 : skip\"] ;",
      "102 [label=\"102 : skip\"] ;", "101 [label=\"101 : skip\"] ;",
      "100 [label=\"100 : DIG\"] ;", "99 [label=\"99 : skip\"] ;",
      "98 [label=\"98 : skip\"] ;", "97 [label=\"97 : skip\"] ;",
      "96 [label=\"96 : skip\"] ;", "95 [label=\"95 : skip\"] ;",
      "94 [label=\"94 : skip\"] ;",
      "93 [label=\"93 : FAILWITH v18\", shape=cds] ;",
      "92 [label=\"92 : v18 := UNIT\", shape=box] ;",
      "91 [label=\"91 : skip\"] ;", "90 [label=\"90 : skip\"] ;",
      "89 [label=\"89 : skip\"] ;", "88 [label=\"88 : skip\"] ;",
      "87 [label=\"87 : skip\"] ;",
      "86 [label=\"86 : FAILWITH v18\", shape=cds] ;",
      "85 [label=\"85 : skip\"] ;",
      "84 [label=\"84 : IF v17\", shape=diamond] ;",
      "83 [label=\"83 : v17 := GEQ v16\", shape=box] ;",
      "82 [label=\"82 : skip\"] ;", "81 [label=\"81 : skip\"] ;",
      "80 [label=\"80 : v16 := COMPARE v13 v5\", shape=box] ;",
      "79 [label=\"79 : skip\"] ;", "78 [label=\"78 : skip\"] ;",
      "77 [label=\"77 : SWAP\"] ;",
      "76 [label=\"76 : v15 := DUP v13\", shape=box] ;",
      "75 [label=\"75 : skip\"] ;", "74 [label=\"74 : skip\"] ;",
      "73 [label=\"73 : skip\"] ;", "72 [label=\"72 : skip\"] ;",
      "71 [label=\"71 : skip\"] ;", "70 [label=\"70 : skip\"] ;",
      "69 [label=\"69 : DIG\"] ;",
      "68 [label=\"68 : v14 := DUP v5\", shape=box] ;",
      "67 [label=\"67 : skip\"] ;", "66 [label=\"66 : skip\"] ;",
      "65 [label=\"65 : skip\"] ;", "64 [label=\"64 : skip\"] ;",
      "63 [label=\"63 : skip\"] ;", "62 [label=\"62 : skip\"] ;",
      "61 [label=\"61 : skip\"] ;",
      "60 [label=\"60 : FAILWITH v12\", shape=cds] ;",
      "59 [label=\"59 : v12 := UNIT\", shape=box] ;",
      "58 [label=\"58 : skip\"] ;", "57 [label=\"57 : skip\"] ;",
      "56 [label=\"56 : skip\"] ;",
      "55 [label=\"55 : FAILWITH v12\", shape=cds] ;",
      "54 [label=\"54 : v13 := unlift_option v11\", shape=box] ;",
      "53 [label=\"53 : skip\"] ;", "52 [label=\"52 : skip\"] ;",
      "51 [label=\"51 : IF_NONE v11\", shape=diamond] ;",
      "50 [label=\"50 : v11 := GET v10 v7\", shape=box] ;",
      "49 [label=\"49 : skip\"] ;", "48 [label=\"48 : skip\"] ;",
      "47 [label=\"47 : v10 := SENDER\", shape=box] ;",
      "46 [label=\"46 : skip\"] ;", "45 [label=\"45 : skip\"] ;",
      "44 [label=\"44 : DIG\"] ;",
      "43 [label=\"43 : v9 := DUP v7\", shape=box] ;",
      "42 [label=\"42 : skip\"] ;", "41 [label=\"41 : skip\"] ;",
      "40 [label=\"40 : skip\"] ;", "39 [label=\"39 : skip\"] ;",
      "38 [label=\"38 : skip\"] ;", "37 [label=\"37 : skip\"] ;",
      "36 [label=\"36 : v8 := CDR v2\", shape=box] ;",
      "35 [label=\"35 : skip\"] ;", "34 [label=\"34 : skip\"] ;",
      "33 [label=\"33 : v7 := CAR v2\", shape=box] ;",
      "32 [label=\"32 : skip\"] ;", "31 [label=\"31 : skip\"] ;",
      "30 [label=\"30 : v6 := DUP v2\", shape=box] ;",
      "29 [label=\"29 : skip\"] ;", "28 [label=\"28 : skip\"] ;",
      "27 [label=\"27 : skip\"] ;", "26 [label=\"26 : skip\"] ;",
      "25 [label=\"25 : skip\"] ;", "24 [label=\"24 : skip\"] ;",
      "23 [label=\"23 : v5 := CDR v1\", shape=box] ;",
      "22 [label=\"22 : skip\"] ;", "21 [label=\"21 : skip\"] ;",
      "20 [label=\"20 : v4 := CAR v1\", shape=box] ;",
      "19 [label=\"19 : skip\"] ;", "18 [label=\"18 : skip\"] ;",
      "17 [label=\"17 : v3 := DUP v1\", shape=box] ;",
      "16 [label=\"16 : skip\"] ;", "15 [label=\"15 : skip\"] ;",
      "14 [label=\"14 : skip\"] ;", "13 [label=\"13 : skip\"] ;",
      "12 [label=\"12 : v2 := CDR param_storage\", shape=box] ;",
      "11 [label=\"11 : skip\"] ;", "10 [label=\"10 : skip\"] ;",
      "9 [label=\"9 : v1 := CAR param_storage\", shape=box] ;",
      "8 [label=\"8 : skip\"] ;", "7 [label=\"7 : skip\"] ;",
      "6 [label=\"6 : v0 := DUP param_storage\", shape=box] ;",
      "5 [label=\"5 : skip\"] ;", "4 [label=\"4 : skip\"] ;",
      "3 [label=\"3 : skip\"] ;", "2 [label=\"2 : skip\"] ;",
      "1 [label=\"1 : MAIN-EXIT : v41\", shape=doubleoctagon] ;",
      "0 [label=\"0 : MAIN-ENTRY\", shape=doubleoctagon] ;"
    ]
  }
}
```

## Specifying Custom Safety Properties

Given a Michelson program without any annotations, MicSE basically attemps to verify the absence of arithmetic overflow.
However, MicSE also supports verification of custom safety properties using the **`#__MICSE_CHECK {(instruction)}`** statement, where **instruction** denotes arbitrary Michelson instructions that produce boolean values. That is, for each user-provided assertion, MicSE attempts to prove that the top of the stack is the true value.
The **`#__MICSE_CHECK {(instruction)}`** statement should be given as a single-line comment. An example usage is given below. 


### Example

``` michelson
parameter (pair (address %receiver) (mutez %value));
storage   (pair (map %balance address mutez) (mutez %totalSupply));
code { 
  UNPAIR; UNPAIR; DIP 2 { UNPAIR };
        # %receiver :: %value :: %balance :: %totalSupply :: []

  DUP 3; SENDER; GET; IF_NONE { FAIL } {};
        # %balance[@sender] :: %receiver :: %value :: %balance :: %totalSupply :: []

  DUP 3; DUP 2; COMPARE; GE; IF {} { FAIL };
        # %balance[@sender] :: %receiver :: %value :: %balance :: %totalSupply :: []

  DIP { DIG 2; DUP 3 }; SUB; SOME; SENDER; UPDATE;
        # %balance' :: %receiver :: %value :: %totalSupply :: []

  DUP; DUP 3; GET; IF_NONE { PUSH mutez 0 } {};
        # %balance'[%receiver] :: %balance' :: %receiver :: %value :: %totalSupply :: []

  DIP { DIG 2 }; ADD;
        # (%balance'[%receiver] + %value) :: %balance' :: %receiver :: %totalSupply :: []


  # [The user-provided assertion below should be a single-line comment]
  #__MICSE_CHECK { \
    DIP {DROP; DROP }; \
        # (%balance'[%receiver] + %value) :: %totalSupply :: []

    COMPARE; \
        # int :: []

    LE };
        # bool :: []
  # [This notation should be in only one line]
        # (%balance'[%receiver] + %value) :: %balance' :: %receiver :: %totalSupply :: []


  SOME; DIG 2; UPDATE;
        # %balance'' :: %totalSupply :: []
  PAIR; NIL operation; PAIR };
        # (pair [] (pair %balance'' %totalSupply)) :: []
```
