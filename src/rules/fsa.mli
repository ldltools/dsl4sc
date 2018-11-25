(* $Id: fsa.mli,v 1.1 2018/11/23 18:05:16 sato Exp sato $ *)

(** generic nondeterministic finite automaton *)
type ('state, 'label) t

val make : unit -> ('state, 'label) t

val initial_get : ('state, 'label) t -> int
val initial_set : ('state, 'label) t -> int -> unit
    (** initial state *)

val final_get : ('state, 'label) t -> int list
val final_set : ('state, 'label) t -> int list -> unit
    (** final states *)

(** state ops *)

val state_get : ('state, 'label) t -> int -> 'state
    (** raise Not_found when a non-existent state is specified *)
val state_set : ('state, 'label) t -> int -> 'state -> unit
    (** create a new state when a non-existent state is specified *)

val state_add : ('state, 'label) t -> 'state -> int
    (** state value can be duplicate *)
val state_del : ('state, 'label) t -> int -> unit
val state_mem : ('state, 'label) t -> int -> bool

val alist_of_states : ('state, 'label) t -> (int * 'state) list

val states_iter : (int -> 'state -> unit) -> ('state, 'label) t -> unit
val states_fold : (int -> 'state -> 'a -> 'a) -> ('state, 'label) t -> 'a -> 'a

(** sigma : label_index -> label *)

val sigma_get : ('state, 'label) t -> int -> 'label
val sigma_set : ('state, 'label) t -> int -> 'label -> unit

val sigma_add : ('state, 'label) t -> 'label -> int
    (** label value is kept unique *)
val sigma_del : ('state, 'label) t -> int -> unit
val sigma_mem : ('state, 'label) t -> int -> bool

val alist_of_sigma : ('state, 'label) t -> (int * 'label) list

(** transition ops *)

val transition_add : ('state, 'label) t -> int -> int option * int -> unit
val transition_del : ('state, 'label) t -> int -> int option * int -> unit
val transition_mem : ('state, 'label) t -> int -> int option * int -> bool

(** delta : state_index -> transitions = [Some label_index, state_index; ...] *)

val delta_get : ('state, 'label) t -> int -> (int option * int) list
val delta_set : ('state, 'label) t -> int -> (int option * int) list -> unit

val alist_of_delta : ('state, 'label) t -> (int * (int option * int) list) list

(** epsilon-elimination / determinization / minimization *)

val eliminate_epsilon : ('state, 'label) t -> ('state, 'label) t

val determinize : ('state, 'label) t -> (int list, 'label) t

val minimize : (int list, 'label) t -> (int list, 'label) t

(** partially ppx-generated *)

val pp_fsa :
    (Format.formatter -> 'state -> unit) ->
    (Format.formatter -> 'label -> unit) ->
    Format.formatter -> ('state, 'label) t -> unit
val show_fsa :
    (Format.formatter -> 'state -> unit) ->
    (Format.formatter -> 'label -> unit) ->
    ('state, 'label) t -> string

val fsa_of_yojson : (Yojson.Safe.json -> ('state, string) Result.result)
  -> (Yojson.Safe.json -> ('label, string) Result.result)
  -> Yojson.Safe.json -> (('state, 'label) t, string) Result.result
val fsa_to_yojson : ('state -> Yojson.Safe.json) -> ('label -> Yojson.Safe.json)
  -> ('state, 'label) t ->  Yojson.Safe.json

(** graphviz *)

val print_in_dot : out_channel -> 
    ('state -> string) ->
    ('label -> string) ->
    ('state, 'label) t -> unit
