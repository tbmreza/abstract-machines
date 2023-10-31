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

(struct S (expr env store addr) #:transparent)
(define default-env (hash))
; (define default-store (hash))  0 mt

(define/contract (inj e) (-> Exp? struct?)
  (S e (hash) 3 4))

(inj add1)
