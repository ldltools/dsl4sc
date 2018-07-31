(* $Id: nfa.ml,v 1.1 2017/10/21 07:42:33 sato Exp sato $ *)

type ('state, 'alphabet) fixed_nfa =
    { states : 'state array;
      sigma : 'alphabet array;
      delta : (int option * int) list array;
      init : int;
      final : (int list) * (int list);
    }
[@@deriving show, yojson]

(** generic nondeterministic finite automaton *)
type ('state, 'alphabet) nfa =
    { (*mutable states : (int * 'state) list;*)
      states : (int, 'state) Hashtbl.t;
        (** state_id -> state *)
      (*mutable sigma : (int * 'alphabet) list;*)
      sigma : (int, 'alphabet) Hashtbl.t;
        (** alphabet_id -> alphabet *)
      mutable sigma_len : int;
      (*mutable delta : (int * (int option * int) list ref) list;*)
      delta : (int, (int option * int) list ref) Hashtbl.t;
        (** delta = [[(state_id, [(alphabet_id, state_id); ..]); ...]  *)
      mutable init : int;
      mutable final : (int list ref) * (int list ref);
        (** true (accepted) or false (rejected) *)
      mutable nnodes : int;
      mutable nedges : int;
      mutable node_max : int;
      mutable edge_max : int;
    }

(*type 'alphabet transition = int * 'alphabet option * int*)
type transition = int * int option * int

(** nfa <-> fixed_dfa *)

let fnfa2nfa (m : ('state, 'alphabet) fixed_nfa) =
  let states = Hashtbl.create (2 * Array.length m.states) in
  Array.iteri (fun i q -> Hashtbl.add states i q) m.states;
  let sigma = Hashtbl.create (2 * Array.length m.sigma) in
  Array.iteri (fun i a -> Hashtbl.add sigma i a) m.sigma;
  let delta = Hashtbl.create (2 * Array.length m.delta) in
  Array.iteri (fun i es -> Hashtbl.add delta i (ref es)) m.delta;
  let m' : ('state, 'alphabet) nfa =
    { states = states;
      sigma = sigma;
      sigma_len = Array.length m.sigma;
      delta = delta;
      init = m.init;
      final = (ref (fst m.final), ref (snd m.final));
      nnodes = Array.length m.states;
      nedges = Array.fold_left (fun n es -> n + List.length es) 0 m.delta;
      node_max = Array.length m.states - 1;
      edge_max = 0;
    }
  in m'

let nfa2fnfa (m : ('state, 'alphabet) nfa) =
  let node_max = Hashtbl.fold (fun i _ n -> max i n) m.states 0 in
  let states : 'state array =
    Array.init (node_max + 1)
      (fun i ->
	if Hashtbl.mem m.states i then Hashtbl.find m.states i else raise Not_found) in
  let sigma_max = Hashtbl.fold (fun i _ n -> max i n) m.sigma 0 in
  let sigma : 'alphabet array =
    Array.init (sigma_max + 1)
      (fun i ->
	if Hashtbl.mem m.sigma i then Hashtbl.find m.sigma i else raise Not_found) in
  let delta : (int option * int) list array =
    Array.init (node_max + 1)
      (fun i ->
	if Hashtbl.mem m.delta i then !(Hashtbl.find m.delta i) else raise Not_found) in
  let m' : ('state, 'alphabet) fixed_nfa =
    { states = states;
      sigma = sigma;
      delta = delta;
      init = m.init;
      final = !(fst m.final), !(snd m.final);
    }
  in m'

(** nfa ctor *)

let pp_nfa pp_state pp_alphabet (fmt : Format.formatter) (m : ('state, 'alphabet) nfa) =
  let m' : ('state, 'alphabet) fixed_nfa = nfa2fnfa m
  in pp_fixed_nfa pp_state pp_alphabet fmt m'

let show_nfa pp_state pp_alphabet (m : ('state, 'alphabet) nfa) = 
   let m' : ('state, 'alphabet) fixed_nfa = nfa2fnfa m
   in show_fixed_nfa pp_state pp_alphabet m'

let nfa_of_yojson
    (state_of_yojson : Yojson.Safe.json -> ('state, string) Result.result)
    (alpha_of_yojson : Yojson.Safe.json -> ('alphabet, string) Result.result)
    (json : Yojson.Safe.json) =
  match fixed_nfa_of_yojson state_of_yojson alpha_of_yojson json with
  | Result.Ok fnfa -> Result.Ok (fnfa2nfa fnfa)
  | Result.Error msg -> Result.Error msg

let nfa_to_yojson state_to_yojson alpha_to_yojson (m : ('state, 'alphabet) nfa) =
  m |> nfa2fnfa |> fixed_nfa_to_yojson state_to_yojson alpha_to_yojson

let make () =
  let m : ('state, 'alphabet) nfa =
    { states = Hashtbl.create 1000;
      sigma = Hashtbl.create 100;
      sigma_len = 0;
      delta = Hashtbl.create 1000;
      init = 0;
      final = (ref [], ref []);
      nnodes = 0;
      nedges = 0;
      node_max = 0;
      edge_max = 0;
    }
  in m

(** nfa state *)

let get_state (m : ('state, 'alphabet) nfa) i =
(*
  assert (List.mem_assoc i m.states);
  List.assoc i m.states
 *)
  assert (Hashtbl.mem m.states i);
  Hashtbl.find m.states i

let mem_state (q : 'state) (m : ('state, 'alphabet) nfa) =
(*
  try
    let _ = List.find (fun (_, q') -> q' = q) m.states in true
  with Not_found -> false
 *)
  try
    Hashtbl.iter (fun _ q' -> if q' = q then raise Exit) m.states;
    false
  with Exit -> true

let state_index (m : ('state, 'alphabet) nfa) (q : 'state) =
  assert (mem_state q m);
(*
  fst (List.find (fun (i, q') -> q' = q) m.states)
 *)
  let i = ref (-1) in
  try
    Hashtbl.iter (fun j q' -> if q' = q then (i := j; raise Exit)) m.states;
    raise Not_found
  with Exit -> !i

let add_state m q i_opt =
(*
  assert (m.nnodes = List.length m.states);
  let i = if m.states = [] then 0 else fst (List.hd (List.rev m.states)) + 1 in
  m.states <- m.states @ [i, q];
  m.nnodes <- m.nnodes + 1;
  i
 *)
  assert (m.nnodes = Hashtbl.length m.states);
  let i =
    match i_opt with
    | None -> m.node_max
    | Some i ->
	assert (not (Hashtbl.mem m.states i));
	i
  in
  Hashtbl.add m.states i q;
  m.nnodes <- m.nnodes + 1;
  m.node_max <- m.node_max + 1;
  i

let rec del_state m i =
  assert (Hashtbl.mem m.states i);
(*
  assert (m.nnodes = List.length m.states);
  assert (List.mem_assoc i m.states);
  m.states <- List.remove_assoc i m.states;
  m.delta <- List.remove_assoc i m.delta;
  List.iter
    (fun (i, es) ->
      let es' =
	List.fold_left
	  (fun rslt (a, j) -> if j = i then rslt else rslt @ [a, j]) [] !es
      in es := es')
    m.delta;
  m.nnodes <- m.nnodes - 1;
  m.nedges <- List.fold_left (fun n (_, es) -> n + List.length !es) 0 m.delta
 *)
  assert (m.nnodes = Hashtbl.length m.states);
  Hashtbl.remove m.states i;
  m.nnodes <- m.nnodes - 1;
  m.nedges <- Hashtbl.fold (fun _ es n -> n + List.length !es) m.delta 0;

  Hashtbl.remove m.delta i;
  Hashtbl.iter
    (fun i es ->
      let es' =
	List.fold_left
	  (fun rslt (a, j) -> if j = i then rslt else rslt @ [a, j]) [] !es
      in es := es')
    m.delta

let set_state (m : ('state, 'alphabet) nfa) (i : int) (q : 'state) =
  let _ =
    if not (Hashtbl.mem m.states i) then add_state m q (Some i) else
    (Hashtbl.replace m.states i q; i)
  in ()

let alist_of_states (m : ('state, 'alphabet) nfa) =
(*
  m.states
 *)
  Hashtbl.fold
    (fun i q rslt -> rslt @ [i, q])
    m.states []

let init (m : ('state, 'alphabet) nfa) =
  m.init

let final (m : ('state, 'alphabet) nfa) =
  m.final

(** nfa transition *)

let rec alphabet_to_index (m : ('state, 'alphabet) nfa) (a : 'alphabet) =
  assert (mem_alphabet a m);
(*
  let i, _ = List.find (fun (_, a') -> a' = a) m.sigma in i
 *)
  let i = ref (-1) in
  try
    Hashtbl.iter (fun j a' -> if a' = a then (i := j; raise Exit)) m.sigma;
    raise Not_found
  with Exit -> !i

and mem_alphabet (a : 'alphabet) m =
(*
  try let _ = List.find (fun (_, a') -> a' = a) m.sigma in true with Not_found -> false
 *)
  try
    Hashtbl.iter (fun _ a' -> if a' = a then raise Exit) m.sigma;
    false
  with Exit -> true

let sigma_index = alphabet_to_index

let mem_transition (i, (a_opt : int option), j) (m : ('state, 'alphabet) nfa) =
(*
  if not (List.mem_assoc i m.delta) then false else
  let es : (int option * int) list ref = List.assoc i m.delta in
  try
    let _ =
      List.find
	(function
	  | _, j' when j' <> j -> false
	  | None, _ -> a_opt = None
	  | Some (a_in_delta : int), _ ->
	      let b =
		match a_opt with
		| None -> false
		| Some (a : int) -> a = a_in_delta
	      in b
	  | _ -> false)
	!es
    in true
  with Not_found -> false
*)
  if not (Hashtbl.mem m.delta i) then false else
  let es : (int option * int) list ref = Hashtbl.find m.delta i in
  try
    let _ =
      List.find
	(function
	  | _, j' when j' <> j -> false
	  | None, _ -> a_opt = None
	  | Some (a_in_delta : int), _ ->
	      let b =
		match a_opt with
		| None -> false
		| Some (a : int) -> a = a_in_delta
	      in b
	  | _ -> false)
	!es
    in true
  with Not_found -> false

let mem_transition2 (i, (a_opt : 'alphabet option), j) (m : ('state, 'alphabet) nfa) =
(*
  if not (List.mem_assoc i m.delta) then false else
  let es : (int option * int) list ref = List.assoc i m.delta in
  try
    let _ =
      List.find
	(function
	  | _, j' when j' <> j -> false
	  | None, _ -> a_opt = None
	  | Some (a_in_delta : int), _ ->
	      let b =
		match a_opt with
		| None -> false
		| Some (a : 'alphabet) when not (mem_alphabet a m) -> false
		| Some (a : 'alphabet) ->
		    assert (mem_alphabet a m);
		    alphabet_to_index m a = a_in_delta
	      in b
	  | _ -> false)
	!es
    in true
  with Not_found -> false
 *)
  if not (Hashtbl.mem m.delta i) then false else
  let es : (int option * int) list ref = Hashtbl.find m.delta i in
  try
    let _ =
      List.find
	(function
	  | _, j' when j' <> j -> false
	  | None, _ -> a_opt = None
	  | Some (a_in_delta : int), _ ->
	      let b =
		match a_opt with
		| None -> false
		| Some (a : 'alphabet) when not (mem_alphabet a m) -> false
		| Some (a : 'alphabet) ->
		    assert (mem_alphabet a m);
		    alphabet_to_index m a = a_in_delta
	      in b
	  | _ -> false)
	!es
    in true
  with Not_found -> false

let add_transition (m : ('state, 'alphabet) nfa) (i, (a_opt : 'alphabet option), j) =
  if mem_transition2 (i, a_opt, j) m then
    match a_opt with None -> None | Some a -> Some (alphabet_to_index m a)
  else
  let a_opt' : int option =
    match a_opt with
    | None -> None
    | Some a when mem_alphabet a m -> Some (alphabet_to_index m a)
   (*
    | Some a when m.sigma = [] ->
	m.sigma <- [0, a]; Some 0
    | Some a ->
	let i, _ = List.hd (List.rev m.sigma)
	in m.sigma <- m.sigma @ [i + 1, a]; Some (i + 1)
    *)
    | Some a ->
	let n = Hashtbl.length m.sigma in
	Hashtbl.add m.sigma n a;
	Some n
  in
  let _ =
    (*
    try
      let es = List.assoc i m.delta in es := !es @ [a_opt', j]
    with Not_found ->
      m.delta <- m.delta @ [i, ref [a_opt', j]]
     *)
    try
      let es = Hashtbl.find m.delta i in es := !es @ [a_opt', j]
    with Not_found ->
      Hashtbl.add m.delta i (ref [a_opt', j])
  in a_opt'

let del_transition (m : ('state, 'alphabet) nfa) (i, (a_opt : int option), j) =
  if not (mem_transition (i, a_opt, j) m) then () else
  (*
  let es : (int option * int) list ref = List.assoc i m.delta in
   *)
  let es : (int option * int) list ref = Hashtbl.find m.delta i in
  let es' : (int option * int) list =
    List.fold_left
      (fun rslt -> function
	| None, j' when a_opt = None && j' = j -> rslt
	| Some a, j' when a_opt = Some a && j' = j -> rslt
	| e -> rslt @ [e])
      [] !es in
  let _ =
    es := es';
    (*
    m.nedges <- List.fold_left (fun rslt (_, es) -> rslt + List.length !es) 0 m.delta;
     *)
    m.nedges <- Hashtbl.fold (fun _ es rslt -> rslt + List.length !es) m.delta 0;
  in ()

(** sigma *)

let alist_of_sigma (m : ('state, 'alphabet) nfa) =
(*
  m.sigma
 *)
  Hashtbl.fold (fun i a rslt -> rslt @ [i, a]) m.sigma []

let sigma_init (m : ('state, 'alphabet) nfa) s =
(*
  m.sigma <- s
 *)
  List.iter (fun (i, a) -> Hashtbl.add m.sigma i a) s

let sigma_mem (m : ('state, 'alphabet) nfa) (a : 'alphabet) =
  try
    Hashtbl.iter (fun _ a' -> if a' = a then raise Exit) m.sigma;
    false
  with Exit -> true

let sigma_get (m : ('state, 'alphabet) nfa) i =
  Hashtbl.find m.sigma i

let sigma_add (m : ('state, 'alphabet) nfa) (a : 'alphabet) =
(*
  let i = List.length m.sigma in
  m.sigma <- m.sigma @ [i, a];
  i
 *)
  let i = Hashtbl.length m.sigma in
  Hashtbl.add m.sigma i a;
  i

let sigma_add2 (m : ('state, 'alphabet) nfa) (i, a) =
(*
  assert (not (List.mem_assoc i m.sigma));
  m.sigma <- m.sigma @ [i, a];
  ()
 *)
  assert (not (Hashtbl.mem m.sigma i));
  Hashtbl.add m.sigma i a;
  ()

let sigma_len (m : ('state, 'alphabet) nfa) =
(*
  List.length m.sigma
 *)
  Hashtbl.length m.sigma

(** delta *)

let delta_get (m : ('state, 'alphabet) nfa) i =
(*
  if not (List.mem_assoc i m.delta) then m.delta <- m.delta @ [i, ref []];
  assert (List.mem_assoc i m.delta);
  let es : (int option * int) list ref = List.assoc i m.delta
  in !es
 *)
  if not (Hashtbl.mem m.delta i) then Hashtbl.add m.delta i (ref []);
  assert (Hashtbl.mem m.delta i);
  let es : (int option * int) list ref = Hashtbl.find m.delta i
  in !es

let delta_set m i es =
(*
  assert (List.mem_assoc i m.delta);
  let es' = List.assoc i m.delta in es' := es
 *)
  assert (Hashtbl.mem m.delta i);
  let es' = Hashtbl.find m.delta i in es' := es

let alist_of_delta m =
(*
  List.map (fun (i, es) -> i, !es) m.delta
 *)
  Hashtbl.fold (fun i es rslt -> rslt @ [i, !es]) m.delta []

let nnodes_get m = m.nnodes
let nnodes_set m n = m.nnodes <- n
let nedges_get m = m.nedges
let nedges_set m n = m.nedges <- n

(** iteration *)

let states_iteri f (m : ('state, 'alphabet) nfa) =
(*
  List.iter
    (fun (i, q) -> f i q)
    m.states
 *)
  Hashtbl.iter f m.states

let states_fold_left f rslt (m : ('state, 'alphabet) nfa) =
(*
  List.fold_left
    f
    rslt m.states
 *)
  Hashtbl.fold
    (fun i q rslt -> f rslt (i, q))
    m.states rslt 

let delta_iteri f (m : ('state, 'alphabet) nfa) =
(*
  List.iter
    (fun (i, es) -> f i es)
    m.delta
 *)
  Hashtbl.iter f m.delta
