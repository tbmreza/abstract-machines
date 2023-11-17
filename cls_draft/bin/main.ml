exception User of string

type index = int
type term = Ind of index
          | Lambda of term
          (* | App of term * term *)
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
let _default_store (a: addr) : value =
        match a with
        | _ -> raise (User "accessing store with nonexistent addr")

type _state = S of c * l * store * s


let _step = function
        (* | State ((Term (Ind 0)) :: c, Env (v :: _) :: l, s) -> *)
        (*   State (c, l, v :: s) *)
        (* control is Ind 0, env points to value of interest. *)

        | S ((Term (Ind 0)) :: c , a :: l, st, _) ->
          S (c, l, st, a)

        | _ -> assert false
