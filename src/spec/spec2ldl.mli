(* $Id: spec2ldl.mli,v 1.1 2017/08/25 20:07:53 sato Exp $ *)

type event_map = (string * Ldl.formula) list

val translate : Spec.t -> Ldl.formula list * event_map
