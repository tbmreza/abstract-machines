# Exercise 1: Galois Connections

## Concept
A **Galois Connection** is the formal framework relating a **concrete domain** (precise semantics) and an **abstract domain** (approximate semantics). It consists of two monotonic functions:
*   **Abstraction ($\alpha$)**: Maps a set of concrete states to an abstract element.
*   **Concretization ($\gamma$)**: Maps an abstract element back to the set of concrete states it represents.

Formally, $(C, \subseteq) \underset{\alpha}{\overset{\gamma}{\leftrightarrows}} (A, \sqsubseteq)$ is a Galois Connection if for all $c \in C$ and $a \in A$:
$$ \alpha(c) \sqsubseteq a \iff c \subseteq \gamma(a) $$

## Goal
Define a Galois Connection for the **Sign Analysis** domain.

## Problem Description
The concrete domain $C$ is the powerset of integers, $\mathcal{P}(\mathbb{Z})$.
The abstract domain $A$ is the set of signs: $Sign = \{ \bot, -, 0, +, \top \}$.

1.  **Define the partial order $\sqsubseteq$ for $Sign$.** Draw the Hasse diagram (lattice structure).
2.  **Define the concretization function $\gamma: Sign \to \mathcal{P}(\mathbb{Z})$.**
    *   Hint: What integers does $+$ represent? What does $\top$ represent?
3.  **Derive the abstraction function $\alpha: \mathcal{P}(\mathbb{Z}) \to Sign$.**
    *   Ensure your definition satisfies the Galois Connection property: $\alpha(S) \sqsubseteq a \iff S \subseteq \gamma(a)$.

## Implementation Task (Optional)
Implement the `Sign` lattice in a language of your choice (e.g., OCaml, Haskell).
*   Define the type for `Sign`.
*   Implement functions for `join` ($\sqcup$), `meet` ($\sqcap$), and `leq` ($\sqsubseteq$).
*   Implement `alpha` and `gamma`.

**Recommended Tool**: OCaml or Haskell.
```ocaml
(* OCaml Hint *)
type sign = Bot | Neg | Zero | Pos | Top
```

```