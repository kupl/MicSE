val intertrx_merge : basic_block:Tz.sym_state -> Tz.sym_state -> Tz.sym_state

val trx_image_equality_fmla : Tz.trx_image -> Tz.trx_image -> Tz.mich_f

val stack_equality_fmlas :
  Tz.mich_cut_category * Tz.mich_cut_category ->
  Tz.sym_image * Tz.sym_image ->
  Tz.mich_f list

val intratrx_merge : basic_block:Tz.sym_state -> Tz.sym_state -> Tz.sym_state
