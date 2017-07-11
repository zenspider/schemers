#lang racket

(require redex)

(define-language PCF
  (e ::=
     x
     (λ (x) e)
     (e e)
     (let ((x e)) e)

     ; booleans
     tt
     ff
     (if e e e)

     ; arithmetic
     n
     (e + e)
     (e - e)

     ; strings
     s
     (e ++ e))

  (n ::=
     natural)

  (s ::=
     string)

  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (λ (x) e #:refers-to x))

(define-extended-language PCF-eval PCF
  ;; (e-top ::= e (err string))
  (e ::=
     ....
     (err string))
  (E-value ::=
     hole
     (E-value e)
     (v E-value)
     (if E-value e e)
     (E-value + e)
     (v + E-value)
     (E-value - e)
     (v - E-value)
     (E-value ++ e)
     (v ++ E-value))
  (v ::=
     n
     s
     tt
     ff
     (λ (x) e)))

(define-metafunction PCF-eval
  ifif : v e e -> e or (err string)
  [(ifif tt e_1 e_2) e_1]
  [(ifif ff e_1 e_2) e_2]
  [(ifif _ _ _) (err "condition is not a boolean")])

(define-metafunction PCF-eval
  plus : v v -> n or (err string)
  [(plus n_1 n_2) ,(+ (term n_1) (term n_2))]
  [(plus n_1 v_2) (err "RHS is not a number")]
  [(plus v_1 v_2) (err "LHS is not a number")])

(define-metafunction PCF-eval
  minus : v v -> n or (err string)
  [(minus n_1 n_2) ,(max 0 (- (term n_1) (term n_2)))]
  [(minus n_1 v_2) (err "RHS is not a number")]
  [(minus v_1 v_2) (err "LHS is not a number")])

(define-metafunction PCF-eval
  append : v v -> s or (err string)
  [(append s_1 s_2) ,(string-append (term s_1) (term s_2))]
  [(append s_1 v_2) (err "RHS is not a string")]
  [(append v_1 v_2) (err "LHS is not a string")])

(define-metafunction PCF-eval
  eval : e -> v or (err string)
  [(eval e) ,(first (apply-reduction-relation* ->value (term e)))])

(define ->value
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-value ((λ (x) e_1) v)) ; x_y to avoid y in grammar
        (in-hole E-value (substitute e_1 x v))
        beta)
   (--> (in-hole E-value (let ((x e_1)) e_2))
        (in-hole E-value ((λ (x) e_2) e_1))
        let)
   (--> (in-hole E-value (if v e_1 e_2))
        (in-hole E-value (ifif v e_1 e_2))
        if)
   (--> (in-hole E-value (v_1 + v_2))
        (in-hole E-value (plus v_1 v_2))
        +)
   (--> (in-hole E-value (v_1 - v_2))
        (in-hole E-value (minus v_1 v_2))
        -)
   (--> (in-hole E-value (v_1 ++ v_2))
        (in-hole E-value (append v_1 v_2))
        ++)))

(module+ test
  (require rackunit)

  (define t1 (term (λ (x) y)))
  (define t2 (term (λ (x) (1 + 1))))
  (define t3 (term (,t2 ,t1)))
  (define t4 (term (if 42 51 80)))
  (define t5 (term (2 - 1)))
  (define t6 (term (1 - 1)))
  (define t7 (term (1 - 2)))
  (define t8 (term (let ((x (λ (a) (λ (b) a))))
                     ((x y) y))))
  (define t9 (term (λ (x) (x (λ (y) (x y))))))

  (define exp8 (term ((λ (x) ((x y) y))
                      (λ (a) (λ (b) a)))))
  (define exp8* (term (((λ (a) (λ (b) a)) y) y)))
  (define exp9 (term (λ (x) ((sd 0) (λ (y) ((sd 1) (sd 0)))))))

  (define e1 (term ("1" + 2)))
  (define e2 (term (1 + "2")))
  (define e3 (term ("1" + "2")))
  (define e4 (term ("1" ++ 2)))
  (define e5 (term (1 ++ "2")))
  (define e6 (term (1 ++ 2)))

  (test-equal (redex-match? PCF (e_1 e_2) t1) #f)
  (test-equal (redex-match? PCF (e_1 e_2) t3) #t)

  (test--> ->value t1)
  (test--> ->value t2)
  (test--> ->value t3 '(1 + 1))
  (test-->> ->value t3 2)
  (test-->> ->value t4 '(err "condition is not a boolean"))
  (test-equal (term (plus 1 1)) 2)
  (test-->> ->value t5 1)
  (test-->> ->value t6 0)
  (test-->> ->value t7 0)
  (test-->  ->value t8 exp8)
  (test-->> ->value t8 exp8*)

  (test--> ->value (term (if tt x y)) (term x))
  (test--> ->value (term (if ff x y)) (term y))
  (test--> ->value (term (1 + 2)) (term 3))
  (test--> ->value (term ("1" ++ "2")) (term "12"))
  (test-equal (term (eval (1 + 2))) 3)

  (test--> ->value e1 (term (err "LHS is not a number")))
  (test--> ->value e2 (term (err "RHS is not a number")))
  (test--> ->value e3 (term (err "LHS is not a number")))
  (test--> ->value e4 (term (err "RHS is not a string")))
  (test--> ->value e5 (term (err "LHS is not a string")))
  (test--> ->value e6 (term (err "LHS is not a string")))

  (test--> ->value t1)
  (test-->> ->value t3 (term 2))

  (displayln 'done)


  ;; ;;-------------------------
  ;; ;; tests
  ;; (test-equal (term (sd* z)) (term z))
  ;; (test-equal (term (sd* (λ(x) y)))
  ;;   (term (λ(x) y)))
  ;; (test-equal (term (sd* (λ(x) x)))
  ;;   (term (λ(x) (sd 0))))
  ;; (test-equal (term (sd* (λ(x) (λ(y) x))))
  ;;   (term (λ(x) (λ(y) (sd 1)))))
  ;; (test-equal (term (sd* (λ (x) (λ (y) y))))
  ;;   (term (λ(x) (λ(y) (sd 0)))))
  ;; (test-equal (term (sd* (λ(z) ((λ(y) (y (λ(x) x))) (λ(x) (z x)))) ))
  ;;   (term      (λ(z) ((λ(y) ((sd 0) (λ(x) (sd 0)))) (λ(x) ((sd 1) (sd 0)))))))
  ;;
  ;; (define left-term     (term (λ(a) ((λ(b)(b a))
  ;;                                    (λ(c) (λ(a) (a (c b))))))))
  ;; (define right-term    (term (λ(b) (b (λ(c) (λ(d) (d b)))))))
  ;;
  ;; (define left-term/sd  (term (λ(a) ((λ(b)((sd 0) (sd 1)))
  ;;                                    (λ(c) (λ(a) ((sd 0) ((sd 1) b))))))))
  ;; (define right-term/sd (term (λ(b) ((sd 0) (λ(c) (λ(d) ((sd 0) (sd 2))))))))
  ;;
  ;; (test-equal (term (sd* (,left-term ,right-term)))
  ;;             (term (,left-term/sd ,right-term/sd)))

  )


;; TODO: f(x) = 42; f((/ 1 0))

;; debugging tips:

;; 1) traces
;;    (traces ->value (term ((λ (x) x) ((λ (y) y) (λ (z) z)))))
;;    (traces ->value (term ((λ (x) 42) ((λ (x) (x x)) (λ (x) (x x))))))
;; 2) copy out + 1 1 and test relation:
;;    (test-equal (redex-match PCF pattern-copied (term ...)))
;;    (redex-match PCF-eval (in-hole E-value (v_1 + v_2)) (term ("1" + 2)))
