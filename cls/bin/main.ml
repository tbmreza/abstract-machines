(* interpreting:  lambda calculus (de bruijn notation)
   using:         CLS machine ([X] original [ ] disentangled)
                  fig.1 of Han91 "Staging transformations for abstract machines" paper

                  C ontrol (list of instructions)
                  L ist of environments
                  S tack of closures  *)

let num = "200"
let () = print_endline num



type term = Ind of int
          | Abs of term
          | App of term * term
let ident = Abs (Ind 0)
let _a = App (ident, ident)

(* DE BRUIJN NOTATION ENCODED EXPRESSIONS *)

(* (lambda (f) (lambda (x) (f (f (f x))))) *)
(* (lambda     (lambda     (1 (1 (1 0))))) *)
(* λ.λ.1(1(10))
*)
(* let _c3 = App ((Ind 1), (Ind 0)) *)
(* let _c3 = App ((Ind 1), App ((Ind 1), App ((Ind 1), (Ind 0)))) *)
(* let _c3 = Abs (App ((Ind 1), App ((Ind 1), App ((Ind 1), (Ind 0))))) *)
let _c3 = Abs (Abs (App ((Ind 1), App ((Ind 1), App ((Ind 1), (Ind 0))))))  (* ?? as_int *)

type instr = Term of term
           | AP  (* special instruction that "moves the computation back to the compilation section." *)

type env = Env of value list
and value = Clo of term * env

let _v = Clo (ident, Env [])

type c = instr list
type l = env list
type s = value list

type state = State of c * l * s
let _s = State ([], [], [])

let inj t = State (t :: [], [], [])

let initial: state = inj (Term ident)

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
*)

let _step = function
        | State ((Term (Abs t)) :: c, e :: l, s) ->
          State (c, l, Clo (t, e) :: s) (* 1 *)

        | State ((Term (App (t0, t1))) :: c, e :: l, s) ->
          State ((Term t0) :: (Term t1) :: AP :: c, e :: e :: l, s) (* 2 *)

        | State ((Term (Ind 0)) :: c, (Env (v :: _)) :: l, s) ->
          State (c, l, v :: s) (* 3 *)

        | State ((Term (Ind n)) :: c, (Env (_ :: e)) :: l, s) ->
          State ((Term (Ind (pred n))) :: c, (Env e) :: l, s) (* 4 *)

        | State (AP :: c, l, v :: Clo (t, (Env e)) :: s) ->
          State ((Term t) :: c, (Env (v :: e)) :: l, s) (* 5 *)

        | _ -> assert false

let unload = function
        | State ([], [], v :: _) -> Some v
        | _ -> None
let _res = unload initial
