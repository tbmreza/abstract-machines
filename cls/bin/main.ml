(* C ontrol (list of instructions) *)
(* L ist of environments *)
(* S tack of closures *)

let num = "200"
let () = print_endline num


type term = Ind of int
          | Abs of term
          (* | App of term * term *)
let ident = Abs (Ind 0)

(* instruction = term | ap *)
type env = int
type value = Closure of term * env
let _v = Closure (ident, 0)

type c = term list
type l = env list
type s = value list

type state = State of c * l * s
let _s = State ([], [], [])

let inj t = State (t :: [], [], [])

let initial: state = inj ident

let unload = function
        | State ([], [], v :: _) -> Some v
        | _ -> None
let _res = unload initial
