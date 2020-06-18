# ProverLib

# Code Layout
The list is topological sorted by its module dependency.

| Module, Type, or Value Name   | Description |
| -------------------------     | ------------|
| `Adt.t`                       | OCaml representation of Michelson. See [the FRESCO-Michelson GitLab page](https://gitlab.com/releaselab/fresco/michelson/-/blob/51cbabede80dcfe77cf960828c9a359e4ae81943/src/lib/adt.mli) for details. |
| `Cfg.t`                       | Control Flow Graph. See [the FRESCO-Tezla-cfg GitLab page](https://gitlab.com/releaselab/fresco/tezla-cfg/-/blob/cb75e7bb56ecba46e3007c7937040a605b288b08/src/lib/flow_graph.ml#L100) for details. |
| `Cg`                          | Call Graph for verification. |
| `Bp`                          | Basic-Path for verification. |
| `Vc`                          | Verification Condition. |