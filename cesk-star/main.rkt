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

(define (any? _) #t)

(define (Exp? v)
  (or (string? v)     ; variable
      (procedure? v)  ; abstraction
      (cons? v)))     ; application

(define (Storable? v)
  (or (any? v)     ; Kont
      (cons? v)))  ; denotable values (lam . env)

(define (Store? v) (hash? v))  ; Map Addr Storable?
(define/contract (lookup m k) (-> hash? any? any?)
  ; Panicking is desired behavior.
  (hash-ref m k))

(define (Kont? v) (list? v))  ; mt | ar(e,r,a) | fn(l,r,a)
(define mt (list))

(struct S (expr env store addr) #:transparent)
(define default-addr 0)
(define default-env (hash))
(define default-store (hash default-addr mt))

(define/contract (inj e) (-> Exp? struct?)
  (S e default-env default-store default-addr))

(define (inj-with r σ e)
  (S e r σ default-addr))

; CHURCH ENCODING

(define ident (lambda (x) x))
(define c1 (lambda (f) (lambda (x) (f x))))
(define c2 (lambda (f) (lambda (x) (f (f x)))))

(define (c->number c)
  ((c add1) 0))
(check-eq? (c->number c2) 2)

(define (cnat n)
  (define (h n acc)
    (match n
      [0 acc]
      [_ `(f ,(h (sub1 n) acc))]))
  (eval `(lambda (f) (lambda (x) ,(h n 'x))) (make-base-namespace)))


; THE MACHINE

(define/contract (final? s) (-> struct? boolean?)
  (match s
    [(S e _r σ a) #:when
                  (and (procedure? e)
                       (empty? (hash-ref σ a #f))) true]
    ; [_ false]))
    [_ true]))
; dont commit
; (check-false (final? (S add1 default-env default-store 1001)))
; (check-true
;     (let ([σ (hash 1001 (list))]) (final? (S add1 default-env σ 1001))))

(define (until p f)
  (define (go x)
    (match x
      [(? p x)  x]
      [_        (go (f x))]))
  go)

(define/contract (fresh-addr store) (-> hash? number?)
  (define (h addr)
    (match (lookup store addr)
      [#f  addr]
      [_   (h (add1 addr))]))
  (h 0))

(define (extend m addr storable)
  (hash-set m addr storable))

(define/contract (step s) (-> struct? struct?)
  (match s
    [(S (? string? e) r σ a)
     (let* ([e-addr  (lookup r e)]
            [d       (lookup σ e-addr)])
       (S (car d) (cdr d) σ a))]

    [(S (? cons? e) r σ a)
     (let* ([e0  (car e)]
            [e1  (cdr e)]
            [b   (fresh-addr σ)]
            [σ/  (extend σ b (list e1 r a))])  ; ?? struct ar fn
       (S e0 r σ/ b))]

    [_ s]))

; ; test ok:  ?? unit
; (let ([r   (hash "x" 1001)]
;       [to  (hash 1001 (cons c1 default-env))])
;       (step (inj-with r to "x")))


(define/contract (eval s) (-> struct? struct?)
  ((until final? step) s))


; -- TESTING

(check-equal?
  (let* ([start  (inj "not_in_env")]
         [final  (eval start)])
    (S-expr final))  ; ?? define control-str
  "not_in_env")  ; ?? expect thunk (lambda () not_in_env)

(let* ([start  (inj (cons ident c2))]
       [final  (eval start)])
  (display 'final:)(displayln final)
  (S-expr final))
