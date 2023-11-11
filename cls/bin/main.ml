(* interpreting:  lambda calculus (de bruijn notation)
   using:         CLS machine ([X] original [ ] disentangled)
                  fig.21 of Han92 "From Operational Semantics to Abstract Machines" paper

                  C ontrol (list of instructions)
                  L ist of environments
                  S tack of closures  *)

let num = "200"
let () = print_endline num



type term = Ind of int
          | Abs of term
          | App of term * term
let ident = Abs (Ind 0)
let ap = ident (* ?? *)
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

(* instruction = term | ap *)

type env = Env of value list
and value = Clo of term * env

let _v = Clo (ident, Env [])

type c = term list
type l = env list
type s = value list

type state = State of c * l * s
let _s = State ([], [], [])

let inj t = State (t :: [], [], [])

let initial: state = inj ident

(* STEPS

First describing the algorithm poorly:
  3. index closest to binder; pop l; push its term to s
  4. index n, pred n; pop l, push its tail back to l; s unchanged
  1. abs head, control tail; pop l; the 2 combine for new s head
  2. app head, reform control with ap sequence; l pushes its own head; s unchanged
  5. ap labeled, control (second s) term; push l with that term * env; (cddr s)

Now for intuition:
  3. 0 means index is closest to its lambda binder. Topmost value in the l can jump to s.
  4. With base condition out of the way, now we look to pred index n. We'll handle the topmost value in the base case, so we have to pop it for now.
  ...

*)

let _step = function
        | State ((Ind 0) :: c, (Env (v :: _)) :: l, s) ->
          State (c, l, v :: s) (* 3 *)

        | State ((Ind n) :: c, (Env (_ :: e)) :: l, s) ->
          State ((Ind (pred n)) :: c, (Env e) :: l, s) (* 4 *)

        | State ((Abs t) :: c, e :: l, s) ->
          State (c, l, Clo (t, e) :: s) (* 1 *)

        | State ((App (t0, t1)) :: c, e :: l, s) ->
          State (t0 :: t1 :: ap :: c, e :: e :: l, s) (* 2 *)

        | State (_ap :: c, l, v :: Clo (t, (Env e)) :: s) ->
          State (t :: c, (Env (v :: e)) :: l, s) (* 5 *)

        | _ -> assert false

let unload = function
        | State ([], [], v :: _) -> Some v
        | _ -> None
let _res = unload initial
