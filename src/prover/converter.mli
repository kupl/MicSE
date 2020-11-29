exception Error of string

exception InvalidConversion_Expr of PreLib.Cfg.expr
exception InvalidConversion_Cond of ProverLib.Bp.cond

module VarComparable : sig
  module Key : sig
    type t = ProverLib.Bp.var
    val compare : t -> t -> int
    val sexp_of_t : t -> Core.Sexp.t
    val t_of_sexp : Core.Sexp.t -> t
  end

  include module type of Key
  include module type of Core.Comparable.Make (Key)
end


module CvUtils : sig
  val to_vtyp : PreLib.Cfg.typ -> ProverLib.Vlang.typ
  val read_type : ProverLib.Vlang.Expr.t -> ProverLib.Vlang.typ
end

module Env : sig
  exception Error of string

  module VarMap = VarComparable.Map   (* Core.Map *)
  type body = {
    cfg : PreLib.Cfg.t;
    varname : ProverLib.Bp.var VarMap.t;        (* Variable-name Map          : Original Variable-name  -> Latest Variable-name *)
    varexpr : ProverLib.Vlang.Expr.t VarMap.t;  (* Variable to Expression Map : Variable-name           -> Expression of Verification Language *)
  }
  type t = body ref

  val newvar_prefix : string
  val gen_nv : ProverLib.Bp.var -> ProverLib.Bp.var
  val get_ov : ProverLib.Bp.var -> ProverLib.Bp.var (* "get_ov" just removes continuous "newvar_prefix"es in front of the given string *)

  val create : Pre.Lib.Cfg.t -> t

  val read_vartype : PreLib.Cfg.ident -> env:t -> ProverLib.Vlang.typ

  val read_varname : ProverLib.Bp.var -> env:t -> ProverLib.Bp.var
  val update_varname : ProverLib.Bp.var -> env:t -> ProverLib.Bp.var (* WARNING: "update_varname" will change the given env data *)

  val read_expr_of_cfgvar : Pre.Lib.Cfg.ident -> env:t -> ProverLib.Vlang.Expr.t
  val update_expr_of_cfgvar : Pre.Lib.Cfg.ident -> ProverLib.Vlang.Expr.t -> env:t -> unit
  val string_of_var_expr_map : t -> string
end

(* main convert functions - variable, michelson datas(literal), and cfg-expressions *)
val create_expr_of_michdata_i : PreLib.Mich.data -> ProverLib.Vlang.typ -> ProverLib.Vlang.Expr.t
val create_expr_of_michdata : PreLib.Mich.data PreLib.Mich.t -> ProverLib.Vlang.typ -> ProverLib.Vlang.Expr.t
val create_expr_of_cfgexpr : Env.t -> PreLib.Cfg.expr -> ProverLib.Vlang.Expr.t
val create_formula_of_cond : Env.t -> ProverLib.Bp.cond -> ProverLib.Vlang.v_formula

val rename_formula : ProverLib.Vlang.t -> cenv:Env.t -> ProverLib.Vlang.t

(* convert differently for each basicpaths *)
val sp : Env.t -> (ProverLib.Vlang.t * ProverLib.Query.t list) -> (ProverLib.Bp.vertex * ProverLib.Bp.inst) -> (ProverLib.Vlang.t * ProverLib.Query.t list)

(* main convert function *)
val convert : ProverLib.Bp.t -> PreLib.Cfg.t -> (ProverLib.Vlang.t * ProverLib.Query.t list)
