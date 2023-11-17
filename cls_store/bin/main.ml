(* interpreting:  lambda calculus (de bruijn notation)
   using:         CLS machine with Store indirection

                  C ontrol (list of instructions)
                  L ist of environments
                  S tack of closures  *)

type index = int
type addr = int

type term = Ind of index
          | Lambda of term
          | App of term * term

(* DE BRUIJN NOTATION ENCODED EXPRESSIONS *)
let ident = Lambda (Ind 0)

(* (lambda (f) (lambda (x) (f (f (f x))))) *)
(* (lambda     (lambda     (1 (1 (1 0))))) *)
(*                     λ.λ. 1 (1 (1 0))    *)

let rec h n acc =
        match n with
        | 0 -> acc
        | p -> (App (Ind 1, (h (pred p) acc)))
let as_church n = Lambda (Lambda (h n (Ind 0)))

(* counts number of applications of (Ind 1)   ?? eval *)
let rec h c x = match (c, x) with
        (* | ((App ((Ind 1), t)), x) -> h t (succ x) *)
        | ((App (Ind _, t)), x) -> h t (succ x)  (* ?? *)
        | ((Lambda t), x) -> h t x
        | _ -> x
let unchurch_num c = h c 0

(* (* (lambda (cn) *) *)
(* (*   (lambda (f) *) *)
(* (*     (lambda (x) (f ((cn f) x))))) *) *)
(* let cSUCC  = Lambda (Lambda (Lambda (App (Ind 1, App (App (Ind 2, Ind 1), Ind 0))))) *)

let cTRUE   = Lambda (Lambda (Ind 1))                            (* (lambda (a) (lambda (_) a)) *)
let cFALSE  = Lambda (Lambda (Ind 0))                            (* (lambda (_) (lambda (b) b)) *)
let cAND    = Lambda (Lambda (App (App (Ind 1, Ind 0), Ind 1)))  (* (lambda (p) (lambda (q) ((p q) p))) *)
let cNOT    = Lambda (App (App (Ind 0, cFALSE), cTRUE))          (* (lambda (b) ((b cFALSE) cTRUE)) *)


type instr = Term of term
           | AP  (* special instruction that "moves the computation back to the compilation section." *)

exception User of string

type env = Env of value list
and value = Clo of term * env
type envS = (index -> addr)
type valueS = Closure of term * envS
let default_env : envS = function
        | _ -> raise (User "accessing env with nonexistent index")

let _default_value = Closure (Ind 0, default_env)

let value_termS v = match v with
        | Closure (t, _) -> t
let value_term v = match v with
        | Clo (t, _) -> t

(* σ lower sigma *)
type store = (addr -> valueS)
let default_store (a: addr) : valueS =
        match a with
        | _ -> raise (User "accessing store with nonexistent addr")

type c = instr list
type l = env list
type s = value list
type state = State of c * l * s
(* quartet *)
type stateS = S of c * envS * store * addr

let inj (t: term) = State (
        (Term t) :: [],
        (Env []) :: [],
        [])

let _injS (t: term) : stateS = S (
        (Term t) :: [],
        default_env,  (* wild first guess. why: not list-wrapped as fn is already extensible *)
        default_store,
        0)


(* STEPS

First spelling out the match cases poorly:
  a. Lambda head, control tail; pop l; the 2 combine for new s head
  b. app head, reform control with ap sequence; l pushes its own head; s unchanged
  c. index closest to binder; pop l; push its term to s
  d. index n, pred n; pop l, push its tail back to l; s unchanged
  e. ap labeled, control (second s) term; push l with that term * env; (cddr s)

Now for intuition:
  a. Abstraction term means that it can jump together with the topmost env to s.
  b. We spread application of terms as its constituents followed with special AP as a marker.
     Now if the state that we entered this step with has e as topmost env, in the next state that e is duplicated to accommodate for the application that we spreaded.
  c. 0 means index is closest to its lambda binder. Topmost value in the l can jump to s.
  d. With base condition specified, now we look to pred index n. We'll handle the topmost value in the base case, so we have to pop it for now.
  e. AP instructs that topmost term in s jumps to c, topmost value jumps to l together with the env beneath it. This step dances with step b.

The function to trace and visualize that the machine can work is `step`:
  utop # #trace step;;
  utop # run (inj input_term);;

*)

let stepS = function
        (* | _ -> S ([], [], default_store, 0) *)
        | _ -> S ([], default_env, default_store, 0)

let step = function
        | State ((Term (Lambda t)) :: c, e :: l, s) ->
          State (c, l, Clo (t, e) :: s)

        | State ((Term (App (t0, t1))) :: c, e :: l, s) ->
          State ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, s)

        | State ((Term (Ind 0)) :: c, Env (v :: _) :: l, s) ->
          State (c, l, v :: s)

        | State ((Term (Ind n)) :: c, Env (_ :: e) :: l, s) ->
          State ((Term (Ind (pred n))) :: c, (Env e) :: l, s)

        | State (AP :: c, l, v :: Clo (t, Env e) :: s) ->
          State ((Term t) :: c, Env (v :: e) :: l, s)

        | _ -> assert false

let unload = function
        | State (_, _, v :: _) -> v
        | _ -> assert false

let unloadS = function
        | S (_, _, st, a) -> st a

let is_final state =
        match state with
        | State ([], [], _ :: []) -> true
        | _ -> false

let is_finalS state =
        match state with
        (* | S ([], [], _,  _ :: []) -> true *)
        (* | S ([], [], _,  0) -> true *)
        | _ -> false

let rec until p f x =
  match x with
  | x when p x -> x
  | _ -> until p f (f x)

let run = until is_final step
let _runS = until is_finalS stepS

(* 1. staged unload *)
let c3 = Lambda (Lambda (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0)))))
let staged_value = Closure (c3, default_env)

let st (q: addr) : valueS =
        match q with
        | 1001 -> staged_value
        | _ -> default_store q

let state_value_numS (s: stateS) : int = (unloadS s) |> value_termS |> unchurch_num
let staged: stateS = S ([], default_env, st, 1001)
let () = assert (state_value_numS staged == 3)

(* 2. shortest inj to unload *)
(* unchurchable ident of c0*)

(* EVALUATOR *)

let eval (t: term) : int = unchurch_num (value_term (unload (run (inj t))))
let unchurch_bool (ctest: term) : bool =
        (* Apply it twice. If it evaluates to 1st argument then it's ocaml true. *)
        (* If it doesn't evaluate to 2nd argument either something must be wrong. *)
        match App (App (ctest, as_church 1), as_church 2) |> eval with
        | 1 -> true
        | 2 -> false
        | _ -> assert false

(* can't see the same way working for num because after this counter function is still needed *)
(* need: cSUCC, c0 *)
(* let unchurch_num (ctest: term) : int = App (App (ctest, as_church 0), cSUCC) *)


(* TESTING *)

let state_value_num (s: state) : int = (unload s) |> value_term |> unchurch_num
let assert_num_eq (t: term) (expect: int) =
        assert (expect == (t |> inj |> run |> state_value_num))
let _dstep (t: term) = t |> inj |> step
let _drun (t: term) = t |> inj |> run

(* unittest unload, unchurch *)
let staged: state = State ([], [], Clo (Lambda (Lambda (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0))))), Env []) :: [])
let () = assert (state_value_num staged == 3)

(* applying ident with x gives x *)
let input1 = App (ident, as_church 2)
let () = assert_num_eq input1 2

(* idempotence *)
let t2 = App (ident, App (ident, App (ident, App (ident, App (ident, App (ident, App (ident, App (ident, as_church 3))))))))
let () = assert_num_eq t2 3

(* bool truth table *)
let assert_true (t: term) = assert (unchurch_bool t)
let assert_false (t: term) = assert (not (unchurch_bool t))

let () = assert_true (App (App (cAND, cTRUE), cTRUE))
let () = assert_false (App (App (cAND, cTRUE), cFALSE))
let () = assert_false (App (App (cAND, cFALSE), cTRUE))
let () = assert_false (App (App (cAND, cFALSE), cFALSE))

let () = assert_false (App (cNOT, cTRUE))
let () = assert_true (App (cNOT, cFALSE))

(* (* succ 0 ok, why: unchurch corner case *) *)
(* let cnat = App (cSUCC, as_church 0) *)
(* let () = assert_num_eq cnat 1 *)
(* let _cnat = App (cSUCC, Lambda (Lambda (App (Ind 1, Ind 0)))) *)
