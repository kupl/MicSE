# Pre-processor Library

## Code Layout

The list is roughly sorted by its module dependency.

| Module, Type, or Value Name | Description                                        |
| --------------------------- | -------------------------------------------------- |
| `Lexer` | Custom Michelson Lexer |
| `Parser` | Custom Michelson Parser; Corresponding datatypes are defined at `Mich` module |
| `Operation.t` | Type to represent Michelson Operation; It might be deprecated now |
| `Mich` | Michelson representation in OCaml |
| `Mich.pos` | Michelson Code Position (#Column & #Line) |
| `Mich.loc` | Michelson Code Location (It can annotate Unknown position, or the start & end of the position) |
| `Mich.annot` | Michelson-defined annotation; There are three types of annotations, `:`, `@`, and `%` |
| `'a Mich.t` | Michelson representation wrapper; It adds `Mich.loc` and `Mich.annot` to `'a`; Three kinds of type used, `Mich.typ Mich.t`, `Mich.inst Mich.t`, and `Mich.data Mich.t` |
| `Mich.typ` | Michelson-defined types |
| `Mich.inst` | Michelson-defined instructions |
| `Mich.data` | Michelson-defined data; As in definition, lambda is first class datatype |
| `Mich.program` | Michelson-defined program; parameter, storage, and code inside |
| `Mich.string_of_pgm_ol` | Get string of `Mich.program` in one-line |
| `Mich.get_d` | Unpack `Mich.t` |
| `Mich.gen_t` | Pack the given value with Unknwon-loc and Empty-annotation |
| `Mich.optm_all_pgm` | Remove redundant expression in `Mich.program`; Especially removes `I_noop` instruction generated from parser |
| `Adt` | Wrapper & Legacy module of `Mich`; It mainly contains parsing procedure |
| `Adt.parse` | Parse Michelson Code |
| `Adt.parse_data` | Parse Michelson Data; Parsing Lambda data is not tested well |
| `Cfg` | Control Flow Graph representation of the Michelson program; It is the core datatype of the Preprocessing Library; It is inspired by [Tezla](https://gitlab.com/releaselab/fresco/tezla) implementation, but our cfg & related implementations are far more precise and fluent than Tezla; WARNING: Cfg is not SSA form |
| `Cfg.typ` | Equal to `Mich.typ Mich.t` |
| `Cfg.data` | Equal to `Mich.data Mich.t` |
| `Cfg.var` | Variable used in Cfg; Just string |
| `Cfg.ident` | Not used; Just string |
| `Cfg.loc` | Equal to `Mich.loc` |
| `Cfg.stmt` | Every control flows in Cfg; Except `E_exec` originated from Michelson instruction `EXEC`; Statement cannot be nested in statement, they are connected by edge-label (conceptually) |
| `Cfg.expr` | Right hand side of `Cfg_assign`; Contains almost every non-control-flow instructions in Michelson, but some are not in Michelson such as `E_hd`, `E_tl`, and `E_unlift_option`; Expression cannot be nested in expression, each calculation steps should be separated using multiple assign statements |
| `Cfg.vertex` | Just integer; It is the identifier for vertex in graph |
| `Cfg.edge_label` | It mainly shows Control Flow, Then and Else branches, and some other flows such as Failure and Skip-MicseCheck |
| `Cfg.V` | Vertex module for ocamlgraph; Includes `Cfg.vertex` |
| `Cfg.E` | Edge module for ocamlgraph; Includes `Cfg.edge_label` |
| `Cfg.G` | `Graph.Persistent.Digraph.ConcreteBidirectionalLabeled (Cfg.V) (Cfg.E)`; this module is generated using functor in ocamlgraph |
| `Cfg.lambda_ident` | Michelson code can deal with function values(lambda values) as data, and (Michelson -> Cfg) translator will convert lambda-value-literals in control flow graph too; Lambda's control flow graph is does not connected with main-function's graph, and we need common name to refer them |
| `Cfg.lambda_summary` | It contains lambda-cfg's entry vertex, exit-vertex, parameter type, and return type |
| `Cfg.t` | Main type of control flow graph |
| `Cfg.t.flow` | `Cfg.G.t`; real control flow information embedded; Lambda value's graph representation also included in flow too |
| `Cfg.t.vertex_info` | `Cfg.vertex -> Cfg.stmt`; It stores statement for each vertex |
| `Cfg.t.type_info` | `Cfg.string -> Cfg.typ`; It stores types for each variable |
| `Cfg.t.main_entry` | vertex identifier of main-function's entry; It has `Cfg_skip` statement when translated from Michelson code |
| `Cfg.t.main_exit` | vertex identifier of main-function's exit; It has `Cfg_assign (x, E_itself x)` (x is same for LHS and RHS) when translated from Michelson code |
| `Cfg.t.adt` | Original Michelson Adt before translated to Cfg |
| `Cfg.t.lambda_id_map` | `Cfg.lambda_ident -> Cfg.lambda_summary`; It stores the information to access each lambda value's graph representation |
| `Cfg.t.fail_vertices` | Collection of the vertices whose statement is `Cfg_failwith` |
| `Cfg.t.pos_info` | Contains the Michelson-code-location information of vertices; Small number of vertices' location informations are enrolled for now |
| `Cfg.param_storage_name` | MAGIC-STRING, variable to indicate `pair(parameter, storage)` data |
| `Cfg.gen_param_name` | MAGIC-STRING; This needed when generating multiple parameter variable(identifier) string in multi-transaction scenario; It is not used in Cfg context |
| `Cfg.cfgcon_ctr` | Shortname for `counter for cfg construction`; Internal datatype which carries integer counters, each counters increase when the procedure creates new identifier; It is recommended to use counters to handle cfg to avoid generating duplicate identifiers |
| `Cfg.Semantic` | Experimental GADT setup to express Michelson Cfg in symbolic way; They are defined but not used in two reasons, because GADT is too hard to use, and it is hard to express semantics between concrete and symbolic way. It helps a lot when redesigning Prover's Verification Lanauge (`Prover.Vlang`) |
| `CfgUtil` | It contains Cfg optimization process and Loop unrolling utilities |
| `CfgUtil.LoopUnrolling` | It unrolls every loop, map, and iter instruction n times; Use `LoopUnrolling.run` for just unrolled cfg, and `LoopUnrolling.unroll` to get (before & after) vertex relationship too |
| `Cg` | Unimplemented now; Will contain call-graph (caller-callee relationship) related functions |
| `PreLib` | Wrapper module for Preprocessing Libraries. You can access modules in this library like `PreLib.Cfg` |
