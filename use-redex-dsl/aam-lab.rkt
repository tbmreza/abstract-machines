#lang racket
(require redex redex/tut-subst)

(define-language L
  [e (e e)
     (λ (x t) e)
     x
     (amb e ...)
     number
     (+ e ...)
     (if0 e e e)
     (fix e)]
  [t ::= (→ t t) num]
  [x variable-not-otherwise-mentioned])

; Exercise 1: _
(let* ([rm (redex-match L
         ((λ _ e) _)  (term ((λ (x num) (+ x 1)) 17)))]
       [b
         (first (match-bindings (first rm)))])
  (test-equal (bind-exp b)
              `(+ x 1)))

; Exercise 2: _1
(let* ([rm (redex-match L
         (→ t_I t_O)  (term (→ num (→ num num))))]
       [bs
         (match-bindings (first rm))])
  (test-equal (map bind-exp bs)
              `(num (→ num num))))

; ; Exercise 3: _ ...
; (redex-match L
;   (_ ... e_1 e_2 _ ...)
;   (term (1 2 3 4)))
;
; ; Exercise 4: _ ..._1
; (redex-match L
;   (_ ..._1 e_left _ ... e_right _ ..._1)
;   (term (1 2 3 4 5)))

(define-extended-language L+Γ L
  [Γ ::= · (x : t Γ)])  ; digraph G* Γ

(define-judgment-form L+Γ
      #:mode (types I I O)
  #:contract (types Γ e t)

  [(types Γ e_1 (→ t_2 t_3))
   (types Γ e_2 t_2)
   -------------------------
   (types Γ (e_1 e_2) t_3)]

  [(types (x : t_1 Γ) e t_2)
   -----------------------------------
   (types Γ (λ (x t_1) e) (→ t_1 t_2))]

  [(types Γ e (→ (→ t_1 t_2) (→ t_1 t_2)))
   ---------------------------------------
   (types Γ (fix e) (→ t_1 t_2))]

  [---------------------
   (types (x : t Γ) x t)]

  [(types Γ x_1 t_1)
   (side-condition (different x_1 x_2))  ; 5
   ------------------------------------
   (types (x_2 : t_2 Γ) x_1 t_1)]

  [(types Γ e num) ...
   -----------------------
   (types Γ (+ e ...) num)]

  [--------------------
   (types Γ number num)]

  [(types Γ e_1 num)
   (types Γ e_2 t)
   (types Γ e_3 t)
   -----------------------------
   (types Γ (if0 e_1 e_2 e_3) t)]

  [(types Γ e num) ...
   --------------------------
   (types Γ (amb e ...) num)])

(define-metafunction L+Γ
  [(different x_1 x_1) #f]
  [(different x_1 x_2) #t])


; Exercise 5: refactoring judgment-form
(define-extended-language L5 L
  [Γ ::= · (x : t Γ)])

(define-judgment-form L5
      #:mode (types5 I I O)
  #:contract (types5 Γ e t)
  [---------------------
   (types5 (x : t Γ) x t)]
  [(types5 Γ x_1 t_1)
   ------------------------------------
   (types5 (x_2 : t_2 Γ) x_1 t_1)])

(test-equal
  (judgment-holds
    (types5 (same : num (same : (→ num num) ·)) same t)
    t)
  (list (term (→ num num)) (term num)))


; Exercise 6: refactoring grammar
; ??

(define-extended-language Ev L+Γ
  (p (e ...))
  (P (e ... E e ...))
  (E (v E)
     (E e)
     (+ v ... E e ...)
     (if0 E e e)
     (fix E)
     hole)
  (v (λ (x t) e)
     (fix v)
     number))

(define-metafunction Ev Σ
  : number ... -> number
  [(Σ number ...)
   ,(apply + (term (number ...)))])

(define-metafunction Ev
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match Ev x))

(define red
  (reduction-relation
   Ev
   #:domain p
   (--> (in-hole P (if0 0 e_1 e_2))
        (in-hole P e_1)
        "if0t")
   (--> (in-hole P (if0 v e_1 e_2))
        (in-hole P e_2)
        (side-condition (not (equal? 0 (term v))))
        "if0f")
   (--> (in-hole P ((fix (λ (x t) e)) v))
        (in-hole P (((λ (x t) e) (fix (λ (x t) e))) v))
        "fix")
   (--> (in-hole P ((λ (x t) e) v))
        (in-hole P (subst x v e))
        "βv")
   (--> (in-hole P (+ number ...))
        (in-hole P (Σ number ...))
        "+")
   (--> (e_1 ... (in-hole E (amb e_2 ...)) e_3 ...)
        (e_1 ... (in-hole E e_2) ... e_3 ...)
        "amb")))

; ; Exercise 7: ?? to pdf because gui glitches
; (traces red
;         (term ((+ (amb 1 2)
;                   (amb 10 20)))))

; ; Exercise 8: f :: number -> (any 0..number)

(test-->>
   red
   (term ((if0 1 2 3)))
   (term (3)))

(test-->>
   red
   (term ((+ (amb 1 2)
             (amb 10 20))))
   (term (11 21 12 22)))

(test-results)
