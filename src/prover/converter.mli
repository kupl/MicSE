exception InvalidSituation of PreLib.Cfg.expr

val newvar_prefix : string
val gen_nv : string -> string

type convert_env_body = {
    cfg : PreLib.Cfg.t;
    varname : (string, string) Core.Map.Poly.t;
}
type convert_env = convert_env_body ref

(* deal with convert_env's varname *)
val get_cur_varname : convert_env -> string -> string
val get_new_varname : convert_env -> string -> string

(* read or generate Vlang.typ value *)
val read_type_cfgvar : convert_env -> PreLib.Cfg.ident -> ProverLib.Vlang.typ
val convert_type : PreLib.Cfg.typ -> ProverLib.Vlang.typ

(* main convert functions - variable, michelson datas(literal), and cfg-expressions *)
val create_var_of_cfgvar : convert_env -> PreLib.Cfg.ident -> ProverLib.Vlang.Expr.t
val create_expr_of_michdata : PreLib.Mich.data PreLib.Mich.t -> ProverLib.Vlang.typ -> ProverLib.Vlang.Expr.t
val create_expr_of_cfgexpr : convert_env -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t

(* special case *)
val create_formula_no_overflow : ProverLib.Vlang.Expr.t -> ProverLib.Vlang.t

(* deal with convert_cond *)
val create_var_in_convert_cond : convert_env -> PreLib.Cfg.ident -> ProverLib.Vlang.Expr.t
val convert_cond : convert_env -> ProverLib.Bp.cond -> ProverLib.Vlang.v_formula

(* convert differently for each basicpaths *)
val sp : convert_env -> (ProverLib.Vlang.t * ProverLib.Query.t list) -> (ProverLib.Bp.vertex * ProverLib.Bp.inst) -> (ProverLib.Vlang.t * ProverLib.Query.t list)

(* main convert function *)
val convert : ProverLib.Bp.t -> PreLib.Cfg.t -> (ProverLib.Vlang.t * ProverLib.Query.t list)
