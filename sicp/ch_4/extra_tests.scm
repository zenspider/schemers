(require rackunit)
(require-library eval)
(import eval)

(define the-global-environment (setup-environment))

(define (interpret exp) (eval exp the-global-environment))

(test-group "myscheme"
  (test-group "self-eval"
    (test 5            (interpret 5))
    (test "hey"        (interpret "hey"))
    (test 666666666222 (interpret 666666666222)))

  (test-group "exprs"
    (test 3  (interpret '(+ 1 2)))
    (test 2  (interpret '(* (- 2 3) (- 4 6))))
    (test 11 (interpret '(+ (* 1 2) (/ 6 2) (* (- 5 4) 2 3)))))

  (test-group "quote"
    (test 2                  (interpret '(quote 2)))
    (test 'hello             (interpret '(quote hello)))
    (test '(jay wizz 2 watt) (interpret '(quote (jay wizz 2 watt)))))

  (test-group "conditionals"
    (test-assert (interpret '(if (= 4 5) false 1)))
    (test-assert (interpret '(if (= 5 5) 1 false)))
    (test-assert (interpret '(if false false true)))
    (test-assert (interpret '(if 1 true false)))

    ;; note: -cond- also tests how -begin- works
    (test-assert (interpret '(cond (false false) (else true))))
    (test-assert (interpret '(cond (true true) (else false))))
    (test-assert (interpret
                  '(cond
                    ((= 5 6) false)
                    ((= 4 5) false)
                    ((= 5 5) true)
                    (else false))))
    (test-assert (interpret
                  '(cond
                    ((= 5 6) false)
                    ((= 4 5) false)
                    ((= 51 5) false)
                    (else (= 1 1))))))

  ;; (test-group "or-and"
  ;;   (test-assert (interpret '(or 1 2 3)))
  ;;   (test 3      (interpret '(or false false 3)))
  ;;   (test false  (interpret '(or false false)))
  ;;
  ;;   (test 3      (interpret '(and 1 2 3)))
  ;;   (test false  (interpret '(and false false 3)))
  ;;   (test false  (interpret '(and false false))))

  (test-group "vars"
    (interpret '(define joe 12))

    (test-assert (= (interpret 'joe) 12))
    (test-assert (= (interpret '(+ joe 2)) 14))
    (test-assert (interpret '(= joe 12)))

    (interpret '(define dave 5))
    (test-assert (= (interpret '(+ joe dave)) 17))
    (test-assert (not (interpret '(= joe dave))))

    ;; (interpret '(set! dave 10))
    ;; (interpret '(set! joe (+ 10 dave)))
    ;; (test-assert (= (interpret '(+ joe dave)) 30))
    )

  ;; simple function definition and application

  (test-group "functions"
    (interpret
     '(define (sum a b)
        (+ a b)))
    (interpret
     '(define (average x y)
        (/ (sum x y) 2)))

    (interpret '(define xx 10))
    (interpret '(define yy 20))
    (test 6 (interpret '(sum 2 4)))
    (test 15 (interpret '(average xx yy)))


                                        ; applying a lambda directly
    (test 20 (interpret
              '((lambda (x y) (+ x y)) 15 5)))

                                        ; define an explicit lambda
    (interpret
     '(define lsum
        (lambda (x y) (+ x y))))
    (test 23 (interpret '(lsum 11 12)))
    ;; (interpret
    ;;  '(set! lsum
    ;;         (lambda (x y) (- x y))))
    ;; (test -1 (interpret '(lsum 11 12)))

                                        ; recursive function
    (interpret
     '(define (rsum x y)
        (if (= y 0)
            x
            (rsum (+ x 1) (- y 1)))))
    (test 11 (interpret '(rsum 5 6)))
    (test 6 (interpret '(rsum 0 6)))
    (test 6 (interpret '(rsum 6 0)))

    ;; returning a function from another function
    (interpret
     '(define (make-adder-func x)
        (lambda (y) (+ x y))))
    (interpret
     '(define add2 (make-adder-func 2)))
    (test 12 (interpret '(add2 xx)))
    (test 14 (interpret '((make-adder-func 4) 10)))

    ;; accepting a function as an argument
    (interpret '(define (apply-twice func val)
                  (func (func val))))
    (test 104 (interpret '(apply-twice add2 100)))
    (test 10000
          (interpret
           '(apply-twice (lambda (x) (* x x)) 10)))

    ;; complex high-order wizardry. -compose- takes two functions, and
    ;; returns a function that is their composition

    (interpret '(define (compose f g) (lambda (x) (f (g x)))))
    (interpret '(define (square x) (* x x)))
    (interpret '(define (inc x) (+ x 1)))
    (test 121 (interpret '((compose square inc) 10)))
    (test 101 (interpret '((compose inc square) 10))))

  ;; (test-group "let"
  ;;   (test 6 (interpret '(let ((a 1) (b 2) (c 3)) (+ a b c)))))
  ;;
  ;; (test-group "let*"
  ;;   (test 39 (interpret
  ;;             '(let* ((x 3)
  ;;                     (y (+ x 2))
  ;;                     (z (+ x y 5)))
  ;;                (* x z)))))
  ;;
  ;; (test-group "named-let"
  ;;   (interpret
  ;;    '(define (fib n)
  ;;       (let fib-iter ((a 1) (b 0) (count n))
  ;;         (if (= count 0)
  ;;             b
  ;;             (fib-iter (+ a b) a (- count 1))))))
  ;;   (test 13 (interpret '(fib 7)))
  ;;   (test 21 (interpret '(fib 8))))

  ;; (test-group "while"
  ;;   (interpret
  ;;    '(define xx 5))
  ;;   (interpret
  ;;    '(define yy 6))
  ;;   (interpret
  ;;    '(while (> xx 0)
  ;;            (begin
  ;;              (set! xx (- xx 1))
  ;;              (set! yy (+ yy 1)))))
  ;;   (test 0 (interpret 'xx))
  ;;   (test 11 (interpret 'yy)))
  )