#lang racket

(require "../lib/testes.rkt")
(require "../lib/utils.rkt")

;;; Exercise 3.80

;; A "series RLC circuit" consists of a resistor, a
;; capacitor, and an inductor connected in series, as shown in *Note
;; Figure 3-36::.  If R, L, and C are the resistance, inductance, and
;; capacitance, then the relations between voltage (v) and current
;; (i) for the three components are described by the equations
;;
;;      v_R = i_R R
;;
;;               d_(i L)
;;      v_L = L ---------
;;                 d t
;;
;;               d v_C
;;      i_C = C -------
;;                d t
;;
;; and the circuit connections dictate the relations
;;
;;      i_R = i_L = -i_C
;;
;;      v_C = v_L + v_R
;;
;; Combining these equations shows that the state of the circuit
;; (summarized by v_C, the voltage across the capacitor, and i_L, the
;; current in the inductor) is described by the pair of differential
;; equations
;;
;;      d v_C        i_L
;;      -----  =  -  ---
;;       d t          C
;;
;;      d i_L      1           R
;;      -----  =  --- v_C  -  --- i_L
;;       d t       L           L
;;
;; The signal-flow diagram representing this system of differential
;; equations is shown in *Note Figure 3-37::.
;;
;; *Figure 3.36:* A series RLC circuit.
;;               + v_R -
;;         i_R
;;      +--->----'\/\/\,--------+
;;      |                       |  i_L
;;     \|/          R          \|/
;;   +  |  i_C                  |_   +
;;     -+-                      __)
;; v_C -+- C                   (_)   v_L
;;      |                       __)
;;   -  |                       |    -
;;      +-----------------------+
;;
;; *Figure 3.37:* A signal-flow diagram for the solution to a series
;; RLC circuit.
;;
;;                       +-------------+
;;      +----------------+  scale: l/L |<--+
;;      |                +-------------+   |
;;      |                                  |
;;      |                +-------------+   |  v_C
;;      |       dv_C +-->|   integral  +---*------>
;;      |            |   +-------------+
;;      |            |        ^
;;      |            |        | v_(C_0)
;;      |            |
;;      |            |   +-------------+
;;      |            +---+ scale: -l/C |<--+
;;      |                +-------------+   |
;;      |  |\__                            |
;;      +->|   \_  di_L  +-------------+   |  i_L
;;         | add_>------>|   integral  +---*------>
;;      +->| __/         +-------------+   |
;;      |  |/                 ^            |
;;      |                     | i_(L_0)    |
;;      |                                  |
;;      |                +-------------+   |
;;      +----------------+ scale: -R/L |<--+
;;                       +-------------+
;;
;; Write a procedure `RLC' that takes as arguments the parameters R, L,
;; and C of the circuit and the time increment dt.  In a manner similar to
;; that of the `RC' procedure of *Note Exercise 3-73::, `RLC' should
;; produce a procedure that takes the initial values of the state
;; variables, v_(C_0) and i_(L_0), and produces a pair (using `cons') of
;; the streams of states v_C and i_L.  Using `RLC', generate the pair of
;; streams that models the behavior of a series RLC circuit with R = 1
;; ohm, C = 0.2 farad, L = 1 henry, dt = 0.1 second, and initial values
;; i_(L_0) = 0 amps and v_(C_0) = 10 volts.

;; (assert-equal x y)
(done)
