# Exercise 2: Widening & Narrowing

## Concept
In domains with **infinite height** (like Intervals), standard Kleene iteration (\(ot\), \(f(ot)\), \(f(f(ot))\), \(\dots\)) might not terminate.
*   **Widening (\(\nabla\))**: An operator used to "jump" up in the lattice to ensure termination. It usually extrapolates the growth of the range.
*   **Narrowing (\(\triangle\))**: An operator used after a fixpoint is approximated (post-widening) to refine the result and recover precision.

## Goal
Apply Widening and Narrowing to the **Interval Analysis** domain.

## Problem Description
Consider the Interval domain where elements are \([l, u]\) with \(l, u \in \mathbb{Z} \cup \{-\infty, +\infty\}
). Consider the program loop:
```c
x = 0;
while (x < 100) {
    x = x + 1;
}
```
The transfer function for the loop body is effectively \(F(I) = [0, 0] \sqcup (I + [1, 1]) \sqcap [-\infty, 99]\).

1.  **Simulate Kleene Iteration**: Try to calculate the fixpoint of \(F\) starting from \(\bot\). Does it terminate quickly?
2.  **Define a Widening Operator \(\nabla\)**:
    *   Standard Interval Widening: If an upper bound is unstable (increasing), jump to \(+\infty\). If a lower bound is unstable (decreasing), jump to \(-\infty\).
3.  **Apply Widening**: Calculate the sequence \(X_0 = \bot, X_{n+1} = X_n \nabla F(X_n)\). Find the limit \(X^\nabla\).
4.  **Apply Narrowing**: Starting from \(X^\nabla\), calculate \(Y_0 = X^\nabla, Y_{n+1} = Y_n \triangle F(Y_n)\). Does this recover the bound $100$?

## Implementation Task
Implement the Interval domain with Widening and Narrowing.
*   **Recommended Tool**: OCaml/Haskell for the solver logic.
*   **Hint**:
    ```haskell
    -- Haskell Hint
    data Interval = Bot | Intv (ExtendedInt, ExtendedInt)
    widen (Intv (l1, u1)) (Intv (l2, u2)) = ...
    ```
