type index = int
type term = Ind of index
          | Lambda of term
          | App of term * term
let ident = Lambda (Ind 0)

let rec h n acc =
  match n with
  | 0 -> acc
  | p -> (App (Ind 1, (h (pred p) acc)))
let as_church n = Lambda (Lambda (h n (Ind 0)))

type instr = Term of term
           | AP  (* special instruction that "moves the computation back to the compilation section." *)

type addr = int
type c = instr list
type l = (addr option) list  (* list of addrs pointing to value; the environment *)
type s = addr list           (* addr list whose head points to state's value *)

type value = Closure of term * l
let value_term v = match v with | Closure (t, _) -> t
let unwrap value_option = match value_option with
  | Some v -> v
  | _ -> assert false

type store = (addr -> value option)
let default_store (_a: addr) : value option = None

type state = S of c * l * store * s

let inj (t: term) : state = S (
  Term t :: [],
  None :: [],
  default_store,
  [])

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

  | S (Term (Ind 0) :: c , Some a :: l, st, _) ->
    S (c, l, st, a :: [])

  (* | State ((Term (Ind n)) :: c, Env (_ :: e) :: l, s) -> *)
  (*   State ((Term (Ind (pred n))) :: c, (Env e) :: l, s) *)
  (* control unactionable, ignore env head *)
  | S (Term (Ind n) :: c, _ :: l, st, s) ->
    S (Term (Ind (pred n)) :: c, l, st, s)

  (* | State ((Term (Lambda t)) :: c, e :: l, s) -> *)
  (*   State (c, l, Clo (t, e) :: s) *)
  (* control abstracting, extend s *)
  | S (Term (Lambda t) :: c, env_addr :: l, st, s) ->
    let fresh = fresh_addr st in
    let v = Closure (t, env_addr :: []) in
    S (c, l, extend st fresh v, fresh :: s)

  (* | State (AP :: c, l, v :: Clo (t, Env e) :: s) -> *)
  (*   State ((Term t) :: c, Env (v :: e) :: l, s) *)
  (* AP recipe: env grows from stack. extraction of v takes place; extension of env; *)
  (* stack is key: its head becomes part of env new head; next [t,e], t becomes ctrl, e becomes other part of env new head *)
  | S (AP :: c, l, st, v_addr :: te_addr :: s) ->
    let v = st v_addr in
    let t, e = match st te_addr with
      | Some (Closure (t, e)) -> t, e
      | _ -> assert false in
    let ve = Closure (v |> unwrap |> value_term, e) in
    let fresh = fresh_addr st in
    S ((Term t) :: c, Some fresh :: l, extend st fresh ve, s)

  (* | State ((Term (App (t0, t1))) :: c, e :: l, s) -> *)
  (*   State ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, s) *)
  (* app constituents; leave st and s *)
  | S ((Term (App (t0, t1))) :: c, e :: l, st, s) ->
    S ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, st, s)

  | _ -> assert false

let rec until p f x =
  match x with
  | x when p x -> x
  | _ -> until p f (f x)

let is_final state =
  match state with
  | S ([], [], _, _ :: []) -> true
  | _ -> false

let run = until is_final step

let unload s = match s with
  | S (_, _, st, a :: []) -> (* syntax for nested match due to how ocaml treats indents *)
    let res = match st a with
      | Some (Closure (t, e)) -> Closure (Lambda t, e)
      | _ -> assert false
    in res
  | _ -> assert false

let rec h c x = match (c, x) with
  | ((App (Ind _, t)), x) -> h t (succ x)
  | ((Lambda t), x) -> h t x
  | _ -> x
and unchurch_num c = h c 0

let go (t: term) : term = (inj t) |> run |> unload |> value_term


(* TESTING *)

let state_value_num (s: state) : int = (unload s) |> value_term |> unchurch_num

let st (q: addr) : value option =
  match q with
  | 1001 -> Some (Closure (as_church 3, []))
  | _ -> default_store q

(* test: staged unload *)
let () =
  let staged: state = S ([], [], st, 1001 :: []) in
  assert (state_value_num staged == 3)

(* test: one step away *)
let () =
  let plated = S ((Term (Ind 0)) :: [] , Some 1001 :: [], st, 0 :: []) in
  assert (3 == (plated |> step |> state_value_num))

(* test: 2 steps away *)
let () =
  let plated = S ((Term (Ind 1)) :: [] , Some 400 :: Some 1001 :: [], st, 0 :: []) in
  assert (3 == (plated |> step |> step |> state_value_num))

(* test: ident of ident *)
(* extensional: num == (unloaded (as_church num)) *)
let () =
  let inp = App (ident, ident) in
  let test (* expect ident's property *) = go inp in
  let cnum = App (test, as_church 3) |> go in
  assert (3 == unchurch_num cnum)
