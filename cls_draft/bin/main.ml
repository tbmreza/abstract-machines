exception User of string

type index = int
type term = Ind of index
          | Lambda of term
          | App of term * term
let ident = Lambda (Ind 0)

type instr = Term of term
           (* | AP  (* special instruction that "moves the computation back to the compilation section." *) *)
let _i = Term ident

type addr = int
type c = instr list
type l = addr list  (* list of addrs pointing to value; the environment *)
type s = addr       (* addr pointing to state's value *)

type value = Closure of term * l
let _v0 = Closure (Ind 0, [])
type store = (addr -> value)
let default_store (a: addr) : value =
        match a with
        | _ -> raise (User "accessing store with nonexistent addr")

type state = S of c * l * store * s


let _step = function
        (* | State ((Term (Ind 0)) :: c, Env (v :: _) :: l, s) -> *)
        (*   State (c, l, v :: s) *)
        (* control is Ind 0, env points to value of interest. *)

        | S ((Term (Ind 0)) :: c , a :: l, st, _) ->
          S (c, l, st, a)

        | _ -> assert false

let unload = function
        | S (_, _, st, a) -> st a
let value_term v = match v with
        | Closure (t, _) -> t
(* counts number of applications of (Ind 1) *)
let rec h c x = match (c, x) with
        (* | ((App ((Ind 1), t)), x) -> h t (succ x) *)
        | ((App (Ind _, t)), x) -> h t (succ x)  (* ?? *)
        | ((Lambda t), x) -> h t x
        | _ -> x
let unchurch_num c = h c 0

(* 1. staged unload *)
let c3 = Lambda (Lambda (App (Ind 1, App (Ind 1, App (Ind 1, Ind 0)))))
let staged_value = Closure (c3, [])

let st (q: addr) : value =
        match q with
        | 1001 -> staged_value
        | _ -> default_store q

let state_value_num (s: state) : int = (unload s) |> value_term |> unchurch_num
let staged: state = S ([], [], st, 1001)
let () = assert (state_value_num staged == 3)
