(* $Id: $ *)

type rule =
    string * string * condition * action
      (* (id, e, c, a) *)

and condition =
    Ldl.formula * string option
      (* (c, script) *)

and action =
    action_unit list * string option
      (* (a, script) *)

and action_unit =
  | Act_ensure of Ldl.formula
	(* post-condition *)
  | Act_raise of string
	(* raise an event *)

val applicable : rule -> Ldl.formula * Ldl.formula -> bool * int

    (** appliable r (w1, w2) examines whether r is applicable
	to a transition between w1 and w2.
	it returns: 0 = inapplicable, 0b1111 (15) = (conditionally) applicable

	Given r = (id, e, c, a), this function returns 0, 1, or 2.
	when it returns 2, it is guaranteed that no runtime checking of c or a
	is needed, since it is statically guaranteed by the following rule,
	which is almost identical with the consequence rule of the Hoare logic.

        w1 -> c(=pre), {c}r{a}, a(=post) -> w2
        ----------------------------------------- (applicable unconditionally)
                      {w1}r{w2}
     *)
