(* interpreting:  lambda calculus (de bruijn notation)
   using:         CLS machine ([X] original [ ] disentangled)
                  fig.1 of Han91 "Staging transformations for abstract machines" paper

                  C ontrol (list of instructions)
                  L ist of environments
                  S tack of closures  *)

type term = Ind of int
          | Abs of term
          | App of term * term
let ident = Abs (Ind 0)

(* DE BRUIJN NOTATION ENCODED EXPRESSIONS *)

(* (lambda (f) (lambda (x) (f (f (f x))))) *)
(* (lambda     (lambda     (1 (1 (1 0))))) *)
(* λ.λ.1(1(10))
*)
let c2 = Abs (Abs (App ((Ind 1), App ((Ind 1), (Ind 0)))))
(* ?? as_church :: int -> term *)
let _c3 = Abs (Abs (App ((Ind 1), App ((Ind 1), App ((Ind 1), (Ind 0))))))

(* counts number of applications of (Ind 1) *)
let rec h c x = match (c, x) with
        | ((App ((Ind 1), t)), x) -> h t (succ x)
        | ((Abs t), x) -> h t x
        | _ -> x
let unchurch c = h c 0


type instr = Term of term
           | AP  (* special instruction that "moves the computation back to the compilation section." *)

type env = Env of value list
and value = Clo of term * env

let value_term v = match v with
        | Clo (t, _) -> t

type c = instr list
type l = env list
type s = value list
type state = State of c * l * s

let inj t: (state) = State (
        (Term t) :: [],
        (Env []) :: [],
        [])


(* STEPS

First spelling out the match cases poorly:
  a. abs head, control tail; pop l; the 2 combine for new s head
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

let step = function
        | State ((Term (Abs t)) :: c, e :: l, s) ->
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

let is_final state =
        match state with
        | State ([], [], _ :: []) -> true
        | _ -> false

let rec until p f x =
  match x with
  | x when p x -> x
  | _ -> until p f (f x)

let run = until is_final step


(* TESTING *)

(* test unload, unchurch *)
let staged: state = State ([], [], Clo (Abs (Abs (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0))))), Env []) :: [])
let got: value = unload staged
let () = assert (unchurch (value_term got) == 3)

(* applying ident with x gives x *)
let t1 = App (ident, c2)
let fin = run (inj t1)
let got: value = unload fin
let () = assert (unchurch (value_term got) == 2)
