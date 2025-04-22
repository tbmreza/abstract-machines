observations I'm not terribly sure about:
- Kont is also Storable along with Value(Env); atomic evaluation can yield Kont
- the adj "abstract" in abstract time-stamped cesk* means we use stack data structures
  in tick and alloc instead of first-class functions
- https://smlhelp.github.io/book/docs/concepts/control-flow/cps is a valuable resource
    - may have the breadth for an instructive material, but writing style is uninspiring
- "compositional interpreter" is umbrella term for interpreters born from maths/fp


A state in the CLS machine is the pair (C, (L, S)) in
which C is a list of instructions (terms to be evaluated), L
is a list of environments and S is a stack of closures (computed values).

(* now values have flowed to both f and g. *)
(* but this time 0cfa doesn't have a value-flow rule for (λ (x) (...)); it does have for (lambda (x) ) *)
(lambda (x) (x x))
((λ (g)
   ((λ (f) (f g)) (λ (x) (x x))))
 (λ (x) x))


(* a kcfa bases its simulation on rules. *)
(* 0cfa begins analyzing the first program with a rule for application (f e), *)
(*         seeing (λ (g) (...)) and (λ (x) x) as f and e respectively. *)
(* the rule says because a value flowed in place of the argument, value then flows to formal parameter g. *)

(* g in turn disseminates (λ (x) x) to its body. *)
(* application (f e) rule is now suited again, with ((λ (f) (f (λ (x) x))) (λ (x) (...))) *)
(* now valued f disseminates (λ (x) (...)) to its body. *)
(* ((λ (x) (x x)) (λ (x) x)) *)



0cfa says that lambda terms only look at themselves to determine where they flow *to*.
it's a lambda abstraction? value flows to it.
it's an operation, with value flowing to the operand? value flows to operator's formal param.
it's an operation, with value flowing to operator's body? value flow to the whole operation.

analyzing the first program, 0cfa has no say about where subterm (λ (x) (x x)) flowed *from*.
that particular example program terminates to identity function. but had the omega term flowed
from operator's place, applied on a value that's also an omega, the whole term evaluation now
loops. 1cfa may have a say about contexts.

what I would like to add while saying something about the 2nd program is this. zooming in to the first f in the body:
((λ (f) (f (...))) (λ (y) y))
k-cfa answers using its rules, to which value does f in the body evaluate. it says that value flowed
to it from the formal param; before that from operand/argument (λ (y) y)

is this about close?

one observation/comment that I have is how hard it is working out the big idea in lambda calculus.
analyzing where values flow in nested lambda terms lured me to think about reduction strategy,
which I assume either mathematically related, or a common conceptual pitfall and nothing in between






(* finally we have ((λ (x) (x x)) (λ (x) x)) *)
(* finally we have ((λ (x) x) (λ (x) x)) *)
(* finally we have (λ (x) x) *)


((λ (f) (f (f (λ (x) x)))) (λ (y) y))
((lambda (f) (f (f (lambda (x) x)))) (lambda (y) y))
