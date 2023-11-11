(* C ontrol (list of instructions) *)
(* L ist of environments *)
(* S tack of closures *)

let num = "200"
let () = print_endline num


type term = Ind of int
          | Abs of term
          (* | App of term * term *)
let ident = Abs (Ind 0)

(* (lambda (f) (lambda (x) x)) *)
(* (lambda (f) (lambda (x) (f x))) *)

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

(* STEPS *)
(* 3. Ident, pop l, push its term to s *)

let _step = function
        | State ((Ind 0) :: c, (Env (v :: _)) :: l, s) -> State (c, l, v :: s) (* 3 *)

        | _ -> assert false

let unload = function
        | State ([], [], v :: _) -> Some v
        | _ -> None
let _res = unload initial
