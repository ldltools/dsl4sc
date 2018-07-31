(* $Id: $ *)

type t
type rule = Ldlrule.rule

type label = string * Ldl.formula list * string list
      (* (id, next_world, event_name list) *)

val read_in : in_channel -> Xml.xml * t * rule list
    (* (props, lts, rules) *)

val update : t -> rule list -> t * (string * string list) list
    (** returns (m, alist)
	where alist is of the form [(rid, [tid; ...]); ..]
     *)

val collect_transitions : t -> (string * label * string) list

val rule_id : rule -> string

val verbose : int ref

(* printing *)

val print_states_in_xml : (string -> unit) -> t -> unit
val print_transitions_in_xml : (string -> unit) -> t -> unit
val print_rules_in_xml : (string -> unit) -> t -> (string * string list) list -> rule list -> unit
    (** print_rules_in_xml out m alist rs
	where alist is of the form [(rid, [tid; ...]); ..]
     *)

val debug_print : t -> unit
val debug_print_rule : rule -> unit
