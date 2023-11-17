(* idea: *)
(* type s = addr       (* addr pointing to state's value *) *)
(* type s = addr list  (* addr list whose head points to state's value *) *)

type index = int
type term = Ind of index
          | Lambda of term
          | App of term * term
let ident = Lambda (Ind 0)

type instr = Term of term
           | AP  (* special instruction that "moves the computation back to the compilation section." *)
let _i = Term ident

type addr = int
type c = instr list
type l = addr list  (* list of addrs pointing to value; the environment *)
(* type s = addr list  (* addr list whose head points to state's value *) *)
type s = addr  (* addr list whose head points to state's value *)

type value = Closure of term * l
let _v0 = Closure (Ind 0, [])
(* type store = (addr -> value) *)
type store = (addr -> value option)
(* let default_store (a: addr) : value = *)
let default_store (_a: addr) : value option =
  None
  (* match a with *)
  (* | _ -> raise (User "accessing store with nonexistent addr") *)

type state = S of c * l * store * s

let _inj (t: term) : state = S (
  (Term t) :: [],
  [],
  default_store,
  0)

let rec h st x =
  match st x with
  | None -> x
  | Some _ -> h st (succ x)
and fresh_addr (st: store) : addr = h st 0

let extend st a v : store =
  function
  | n when n == a -> Some v
  | n -> st n

let step = function
  (* | State ((Term (Ind 0)) :: c, Env (v :: _) :: l, s) -> *)
  (*   State (c, l, v :: s) *)
  (* control is Ind 0, env points to value of interest. *)

  | S ((Term (Ind 0)) :: c , a :: l, st, _) ->
    S (c, l, st, a)

  (* | State ((Term (Ind n)) :: c, Env (_ :: e) :: l, s) -> *)
  (*   State ((Term (Ind (pred n))) :: c, (Env e) :: l, s) *)
  (* control unactionable, ignore env head *)
  | S ((Term (Ind n)) :: c, _ :: l, st, s) ->
    S ((Term (Ind (pred n))) :: c, l, st, s)

  (* | State ((Term (Lambda t)) :: c, e :: l, s) -> *)
  (*   State (c, l, Clo (t, e) :: s) *)
  (* control abstracting, extend s *)
  | S ((Term (Lambda t)) :: c, env_addr :: l, st, _s) ->
    let fresh = fresh_addr st in
    let v = Closure (t, env_addr :: []) in
    S (c, l, extend st fresh v, fresh)

  (* (* | State (AP :: c, l, v :: Clo (t, Env e) :: s) -> *) *)
  (* (*   State ((Term t) :: c, Env (v :: e) :: l, s) *) *)
  (* (* AP recipe: env grows from stack. extraction of v takes place; extension of env; *) *)
  (* (* stack is key: its head becomes part of env new head; next [t,e], t becomes ctrl, e becomes other part of env new head *) *)
  (* | S (AP :: c, l, st, v_addr :: te_addr :: s) -> *)
  (*   (* let spreaded query in *) *)
  (*   S ((Term t) :: c, Env (v :: e) :: l, s) *)

  (* | State ((Term (App (t0, t1))) :: c, e :: l, s) -> *)
  (*   State ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, s) *)
  (* app constituents; leave st and s *)
  | S ((Term (App (t0, t1))) :: c, e :: l, st, s) ->
    S ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, st, s)

  | _ -> assert false

let unload = function
  (* | S (_, _, st, a :: []) -> *)
  | S (_, _, st, a) ->
    match st a with
    | Some (Closure (t, e)) -> Closure (Lambda t, e)
    | _ -> assert false

let value_term v = match v with
  | Closure (t, _) -> t
(* counts number of applications of (Ind 1) *)
let rec h c x = match (c, x) with
  (* | ((App ((Ind 1), t)), x) -> h t (succ x) *)
  | ((App (Ind _, t)), x) -> h t (succ x)  (* ?? *)
  | ((Lambda t), x) -> h t x
  | _ -> x
and unchurch_num c = h c 0

(* staged unload *)
let c3 = Lambda (Lambda (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0)))))
let staged_value = Closure (c3, [])

let st (q: addr) : value option =
        match q with
        | 1001 -> Some staged_value
        | _ -> default_store q

let state_value_num (s: state) : int = (unload s) |> value_term |> unchurch_num
let staged: state = S ([], [], st, 1001)
let () = assert (state_value_num staged == 3)

(* one step away *)
let plated = S ((Term (Ind 0)) :: [] , 1001 :: [], st, 0)
let () = assert (3 == (plated |> step |> state_value_num))

(* 2 steps away *)
let plated = S ((Term (Ind 1)) :: [] , 400 :: 1001 :: [], st, 0)
let () = assert (3 == (plated |> step |> step |> state_value_num))

(* ??. end to end *)
(* let inp =  ?? precursor to plated *)
(* let () = assert (3 == (inp |> inj |> step |> state_value_num)) *)
