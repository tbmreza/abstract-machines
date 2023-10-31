#lang racket

(provide (all-defined-out))

; INTERPRETED LANGUAGE: LAMBDA CALCULUS

;; observations I'm not terribly sure about:
; Kont is also Storable along with Value(Env) -> atomic evaluation can yield Kont

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
(define mt 0)
(define default-addr 0)
(define default-env (hash))
(define default-store (hash default-addr mt))

(define/contract (inj e) (-> Exp? struct?)
  (S e default-env default-store default-addr))

; (inj add1)


; THE MACHINE

(define/contract (final? s) (-> struct? boolean?)
  (match s
    [(S e _r t a) #:when
                  (and (procedure? e)
                       (empty? (hash-ref t a #f))) true]
    [_ false]))

(define str (hash 1001 (list)))

(final? (S add1 default-env default-store 1001))
(final? (S add1 default-env str 1001))
