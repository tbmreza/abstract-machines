#lang racket

(provide (all-defined-out))
(require rackunit)

#|

interpreting:  lambda calculus
using:         CESK* machine (tick/alloc: [X] none  [ ] functions  [ ] stacks)

observations I'm not terribly sure about:
- Kont is also Storable along with Value(Env); atomic evaluation can yield Kont
- the adj "abstract" in abstract time-stamped cesk* means we use stack data structures
  in tick and alloc instead of first-class functions

|#

(define (any? _) #t)

(define (Exp? v)
  (or (string? v)     ; variable
      (procedure? v)  ; abstraction
      (cons? v)))     ; application

(define (Storable? v)
  (or (any? v)     ; Kont
      (cons? v)))  ; denotable values (val . env)

(define (Kont? v) (list? v))  ; mt | ar(e,r,a) | fn(l,r,a)

(struct S (expr env store addr) #:transparent)
(define mt (list))
(define default-addr 0)
(define default-env (hash))
(define default-store (hash default-addr mt))

(define/contract (inj e) (-> Exp? struct?)
  (S e default-env default-store default-addr))


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

(define/contract (step s) (-> struct? struct?)
  s)

(define/contract (eval s) (-> struct? struct?)
  ((until final? step) s))

; -- TESTING

(check-equal?
  "not_in_env"  ; ?? "(lambda () not_in_env)"
  (let* ([start  (inj "not_in_env")]
         [final  (eval start)])
    (S-expr final)))  ; ?? define control-str

; r1 :: Env
; r1 "eleven" = Clo (asChurchNum 11, defaultEnv)
; s1 = (Ref "eleven", r1, Mt)
; -- ghci> unchurch $ eval s1
; -- 11

; s2 = inject (Lam ident :@ (Lam $ asChurchNum 5))
; -- ghci> unchurch $ eval s2
; -- 5
