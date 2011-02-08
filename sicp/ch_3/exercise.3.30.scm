
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
;; to propagate.  What is the delay needed to obtain the complete
;; output from an n-bit ripple-carry adder, expressed in terms of the
;; delays for and-gates, or-gates, and inverters?
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
            (fulladder a c-in b s c-out)
            c-out))
        c a-s b-s s-s))
