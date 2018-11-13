(* $Id: nfa.mli,v 1.1 2017/10/21 07:42:33 sato Exp sato $ *)

(** generic nondeterministic finite automaton *)
type ('state, 'alphabet) nfa

(*type 'alphabet transition = int * 'alphabet option * int*)
type transition = int * int option * int

val make : unit -> ('state, 'alphabet) nfa

(** state *)

val get_state : ('state, 'alphabet) nfa -> int -> 'state
val set_state : ('state, 'alphabet) nfa -> int -> 'state -> unit
val state_index : ('state, 'alphabet) nfa -> 'state -> int

val add_state : ('state, 'alphabet) nfa -> 'state -> int option -> int
    (** returns a new (or existing) state index *)
val del_state : ('state, 'alphabet) nfa -> int -> unit
val mem_state : 'state -> ('state, 'alphabet) nfa -> bool

val alist_of_states : ('state, 'alphabet) nfa -> (int * 'state) list

val init : ('state, 'alphabet) nfa -> int
val final : ('state, 'alphabet) nfa -> (int list ref) * (int list ref)

(** transition *)

val add_transition : ('state, 'alphabet) nfa -> int * 'alphabet option * int -> int option
    (** returns a value of (int option) that corresponds with the second argument *)
val del_transition : ('state, 'alphabet) nfa -> transition -> unit
val mem_transition : transition -> ('state, 'alphabet) nfa -> bool

(** sigma *)

val alist_of_sigma : ('state, 'alphabet) nfa -> (int * 'alphabet) list
val sigma_init : ('state, 'alphabet) nfa -> (int * 'alphabet) list -> unit
val sigma_mem : ('state, 'alphabet) nfa -> 'alphabet -> bool
val sigma_get : ('state, 'alphabet) nfa -> int -> 'alphabet
val sigma_index : ('state, 'alphabet) nfa -> 'alphabet -> int
val sigma_add : ('state, 'alphabet) nfa -> 'alphabet -> int
val sigma_add2 : ('state, 'alphabet) nfa -> int * 'alphabet -> unit
val sigma_len : ('state, 'alphabet) nfa -> int

(** delta *)

val delta_get : ('state, 'alphabet) nfa -> int -> (int option * int) list
val delta_set : ('state, 'alphabet) nfa -> int -> (int option * int) list -> unit

val alist_of_delta : ('state, 'alphabet) nfa -> (int * (int option * int) list) list

val nnodes_get : ('state, 'alphabet) nfa -> int
val nnodes_set : ('state, 'alphabet) nfa -> int -> unit
val nedges_get : ('state, 'alphabet) nfa -> int
val nedges_set : ('state, 'alphabet) nfa -> int -> unit

(** iteration *)

val states_iteri : (int -> 'state -> unit) -> ('state, 'alphabet) nfa -> unit

val states_fold_left : ('a -> int * 'state -> 'a) -> 'a -> ('state, 'alphabet) nfa -> 'a

val delta_iteri : (int -> (int option * int) list ref -> unit) -> ('state, 'alphabet) nfa -> unit

(** ppx-generated *)

val pp_nfa :
    (Format.formatter -> 'state -> unit) ->
    (Format.formatter -> 'alphabet -> unit) ->
    Format.formatter -> ('state, 'alphabet) nfa -> unit
val show_nfa :
    (Format.formatter -> 'state -> unit) ->
    (Format.formatter -> 'alphabet -> unit) ->
    ('state, 'alphabet) nfa -> string

val nfa_of_yojson : (Yojson.Safe.json -> ('state, string) Result.result)
  -> (Yojson.Safe.json -> ('alphabet, string) Result.result)
  -> Yojson.Safe.json -> (('state, 'alphabet) nfa, string) Result.result
val nfa_to_yojson : ('state -> Yojson.Safe.json) -> ('alphabet -> Yojson.Safe.json)
  -> ('state, 'alphabet) nfa ->  Yojson.Safe.json
