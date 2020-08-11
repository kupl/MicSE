# Prover

## Code Layout

The list is roughly sorted by its module dependency.

| Module, Type, or Value Name | Description                                                           |
| --------------------------- | --------------------------------------------------------------------- |
| `Extractor`                 | Extract basic path from CFG                                           |
| `Converter`                 | Convert basic path to verification condition by verification language |
| `Verifier`                  | Solve verification condition by SMT solver                            |
| `Generator`                 | Generate proper invariant to verify correctly                         |
