#lang racket

(provide (all-defined-out))
(require rackunit)

#|

interpreting:  lambda calculus
using:         CESK* machine (tick/alloc: [X] none  [ ] functions  [ ] stacks)
               fig.3 of Abstracting Abstract Machines paper

observations I'm not terribly sure about:
- Kont is also Storable along with Value(Env); atomic evaluation can yield Kont
- the adj "abstract" in abstract time-stamped cesk* means we use stack data structures
  in tick and alloc instead of first-class functions
- https://smlhelp.github.io/book/docs/concepts/control-flow/cps
  will provide extra context after implementing second def

|#

(define variable-name? string?)
(define church-encoded? list?)
(struct app (e0 e1))  ; would use cons if `(cons? (list 1 1 1))` weren't true

(struct clo (lam env))

(define (lookup m k)
  (hash-ref m k #f))

(struct ar (e r a) #:transparent)
(struct fn (l r a) #:transparent)

(struct S (expr env store addr) #:transparent)  ; ?? types,guard at least in struct inst
(define default-addr 0)
(define default-env (hash))
(define default-store (hash default-addr empty))

(define (inj e)
  (S e default-env default-store default-addr))

(define (inj-with r σ e)
  (S e r σ default-addr))


; CHURCH ENCODING

(define (church-encoded->procedure data) (eval data (make-base-namespace)))

(define ident `(lambda (x) x))
(define c2 `(lambda (f) (lambda (x) (f (f x)))))

(define (c->number c)
  (((church-encoded->procedure c) add1) 0))
(check-eq? (c->number c2) 2)

(define (cnat n)
  (define (h n acc)
    (match n
      [0 acc]
      [_ `(f ,(h (sub1 n) acc))]))
  `(lambda (f) (lambda (x) ,(h n 'x))))

(struct decapped (var body) #:transparent)

; c2
(define (decap lxe)
  (match lxe
    [`(lambda (,x) ,e)
      (decapped (format "~a" x) e)]))
; (decap c2)  ; ok


; THE MACHINE

(define (final? s)
  (match s
    [(S e _r σ a)
     (let* ([interpretable?  (not (app? e))]
            [store-a         (lookup σ a)]
            [mt?             (or (not store-a) (empty? store-a))])
       (and interpretable? mt?))]))

(define (until p f)
  (define (go x)
    (match x
      [(? p x)  x]
      [_        (go (f x))]))
  go)

(define (fresh-addr store)
  (define (h addr)
    (match (lookup store addr)
      [#f  addr]
      [_   (h (add1 addr))]))
  (h 0))

(define (extend m addr storable)
  (hash-set m addr storable))

(define (step s)
  (display 'input:)(displayln s)
  (define res (match s
    [(S (? variable-name? e) r σ a)
     (displayln "var...")
     (let* ([e-addr  (lookup r e)]
            [d       (lookup σ e-addr)])
       (S (car d) (cdr d) σ a))]

    [(S (? app? e) r σ a)
     (displayln "appl...")
     (let* (
            [e0  (app-e0 e)]
            [e1  (app-e1 e)]
            [b   (fresh-addr σ)]
            [k   (ar e1 r a)]
            [σ+  (extend σ b k)])
       (S e0 r σ+ b))]

    [(S v r σ a)
     (match (lookup σ a)
       [(ar e r+ c)
        (displayln "ar...")
        (let* ([b   (fresh-addr σ)]
               [k   (fn v r c)]
               [σ+  (extend σ b k)])
          (S e r+ σ+ b))]

       [(fn l r+ c)
        (displayln "fn...")
        (let* (
               [xe   (decap l)]
               [x    (decapped-var xe)]
               [e    (decapped-body xe)]
               [b    (fresh-addr σ)]
               [r++  (extend r+ x b)]
               [σ+   (extend σ b (clo v r))])
          (S e r++ σ+ c))])]))
  (display 'state:)(displayln res)(displayln"")
  res)

(define run (until final? step))

(define (eval-clo lam-env)
  ; ?? lambdas without non-local variables. body makes no reference to env
  (clo-lam lam-env))

(define (interpret-control-str state)
  (match state
    [(S (? variable-name? var) r σ _a) #:when (hash-has-key? r var)
     (let* ([var-addr   (lookup r var)]
            [var-d      (lookup σ var-addr)]
            [var-value  (eval-clo var-d)])
       (c->number var-value))]

    [(S (? church-encoded? e) _r _σ _a)
     (c->number e)]

    [(S e _r _σ _a) (format "~a" e)]))


; -- TESTING

(check-equal?
  (let* ([start  (inj "not_in_env")]
         [final  (run start)])
    (interpret-control-str final))
  "not_in_env")

(check-equal?
  (let* ([r      (hash "x" 1001)]
         [σ      (hash 1001 (clo (cnat 2) default-env))]
         [final  (run (inj-with r σ "x"))])
    (interpret-control-str final))
  2)


(let* ([start  (inj (app ident (cnat 2)))]
       ; [fwd  (step start)]
       [final  (run start)]
       )
  (interpret-control-str final)
  ; (void)
  )
