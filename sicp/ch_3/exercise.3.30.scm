#lang racket/base

(require "../lib/queue.scm")
(require "../lib/circuits.scm")
(require "../lib/test.rkt")
(require (only-in srfi/1 fold))

;;; Exercise 3.30

;; *Note Figure 3-27:: shows a "ripple-carry adder"
;; formed by stringing together n full-adders.  This is the simplest
;; form of parallel adder for adding two n-bit binary numbers.  The
;; inputs A_1, A_2, A_3, ..., A_n and B_1, B_2, B_3, ..., B_n are the
;; two binary numbers to be added (each A_k and B_k is a 0 or a 1).
;; The circuit generates S_1, S_2, S_3, ..., S_n, the n bits of the
;; sum, and C, the carry from the addition.  Write a procedure
;; `ripple-carry-adder' that generates this circuit.  The procedure
;; should take as arguments three lists of n wires each--the A_k, the
;; B_k, and the S_k--and also another wire C.  The major drawback of
;; the ripple-carry adder is the need to wait for the carry signals
;; to propagate.
;;
;; *Figure 3.27:* A ripple-carry adder for n-bit numbers.
;;
;;         :                                              :   :
;;         : A_1 B_1   C_1   A_2 B_2   C_2   A_3 B_3   C_3:   : A_n B_n C_n=0
;;         :  |   |   +---+   |   |   +---+   |   |   +-----  :  |   |   +-
;;         |  |   |   |   |   |   |   |   |   |   |   |   :   :  |   |   |
;;         : ++---+---++  |  ++---+---++  |  ++---+---++  :   : ++---+---++
;;         : |   FA    |  |  |   FA    |  |  |   FA    |  :   : |   FA    |
;;         : +--+---+--+  |  +--+---+--+  |  +--+---+--+  :   : +--+---+--+
;;         :    |   |     |     |   |     |     |   |     :   :    |   |
;;      C ------+   |     +-----+   |     +-----+   |     :  ------+   |
;;         :        |       C_1     |       C_2     |     :   :C_(n-1) |
;;         :        |               |               |     :   :        |
;;                 S_1             S_2             S_3                S_n

;; in ruby:
;;
;;   a_s.zip(b_s, s_s).inject(c) { |c_in, (a, b, s)|
;;     c_out = make_wire
;;     full_adder(a, b, c_in, s, c_out)
;;     c_out
;;   }

(define (ripple-carry-adder a-s b-s s-s c)
  (fold (lambda (a b s c-in)
          (let ((c-out (make-wire)))
            (full-adder a c-in b s c-out)
            c-out))
        c a-s b-s s-s))

;; What is the delay needed to obtain the complete output from an
;; n-bit ripple-carry adder, expressed in terms of the delays for
;; and-gates, or-gates, and inverters?

;; A: full adder cost is 2 * half adder + or
;;    half adder is (max and or) + not + and
;;    so ripple would be: n * (2 * ((max and or) + not + and) + or)
;;    or 2n * (max and or) + 2n * not + 2n * and + n * or

(test-group "3.3.0"
            (define a1 (make-wire))
            (define a2 (make-wire))
            (define b1 (make-wire))
            (define b2 (make-wire))
            (define s1 (make-wire))
            (define s2 (make-wire))
            (define c (make-wire))
            (define A (list a1 a2))
            (define B (list b1 b2))
            (define S (list s1 s2))

            (set-signal! (ripple-carry-adder A B S c) 0)

            (test-group "01 + 00 = 001"
                        (set-signal! a1 1)
                        (propagate)

                        (test 1 (get-signal s1))
                        (test 0 (get-signal s2))
                        (test 0 (get-signal c)))

            (test-group "11 + 00 = 011"
                        (set-signal! a2 1)
                        (propagate)

                        (test 1 (get-signal s1))
                        (test 1 (get-signal s2))
                        (test 0 (get-signal c)))

            (test-group "11 + 11 = 110"
                        (set-signal! b1 1)
                        (set-signal! b2 1)
                        (propagate)

                        (test 0 (get-signal s1))
                        (test 1 (get-signal s2))
                        (test 1 (get-signal c))))
