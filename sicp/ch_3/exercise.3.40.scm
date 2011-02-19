
(require 'testes)
(import testes)
(require 'myutils)
(import myutils)

;;; Exercise 3.40

;; Give all possible values of `x' that can result
;; from executing
;;
;;      (define x 10)
;;
;;      (parallel-execute (lambda () (set! x (* x x)))
;;                        (lambda () (set! x (* x x x))))

;; relabel as:
;;      (parallel-execute (lambda () (set! W1 (* R11 R12)))
;;                        (lambda () (set! W2 (* R21 R22 R23))))
;;
;; calculate all combinations via:

;; x = 10
;; r11 = r12 = r21 = r22 = r23 = nil
;;
;; a = %w(W1 R11 R12 W2 R21 R22 R23)
;;
;; l = {
;;   "W1"  => lambda { x = r11 * r12 },
;;   "W2"  => lambda { x = r21 * r22 * r23 },
;;   "R11" => lambda { r11 = x },
;;   "R12" => lambda { r12 = x },
;;   "R21" => lambda { r21 = x },
;;   "R22" => lambda { r22 = x },
;;   "R23" => lambda { r23 = x },
;; }
;;
;; combos = a.permutation.to_a.select { |a|
;;   x = [a.index("R11"), a.index("R12"), a.index("W1")];
;;   y = [a.index("R21"), a.index("R22"), a.index("R23"), a.index("W2")];
;;   x.sort == x && y.sort == y
;; }
;;
;; pp Hash[combos.map { |a| x = 10; a.each { |k| l[k][] }; [a.join(","), x] }]
;;
;; {
;;  "R11,R12,R21,R22,R23,W1,W2" => 1000,
;;  "R11,R12,R21,R22,R23,W2,W1" => 100,
;;  "R11,R12,R21,R22,W1,R23,W2" => 10000,
;;  "R11,R12,R21,W1,R22,R23,W2" => 100000,
;;  "R11,R12,W1,R21,R22,R23,W2" => 1000000
;;  "R11,R21,R12,R22,R23,W1,W2" => 1000,
;;  "R11,R21,R12,R22,R23,W2,W1" => 100,
;;  "R11,R21,R12,R22,W1,R23,W2" => 10000,
;;  "R11,R21,R12,W1,R22,R23,W2" => 100000,
;;  "R11,R21,R22,R12,R23,W1,W2" => 1000,
;;  "R11,R21,R22,R12,R23,W2,W1" => 100,
;;  "R11,R21,R22,R12,W1,R23,W2" => 10000,
;;  "R11,R21,R22,R23,R12,W1,W2" => 1000,
;;  "R11,R21,R22,R23,R12,W2,W1" => 100,
;;  "R11,R21,R22,R23,W2,R12,W1" => 10000,
;;  "R21,R11,R12,R22,R23,W1,W2" => 1000,
;;  "R21,R11,R12,R22,R23,W2,W1" => 100,
;;  "R21,R11,R12,R22,W1,R23,W2" => 10000,
;;  "R21,R11,R12,W1,R22,R23,W2" => 100000,
;;  "R21,R11,R22,R12,R23,W1,W2" => 1000,
;;  "R21,R11,R22,R12,R23,W2,W1" => 100,
;;  "R21,R11,R22,R12,W1,R23,W2" => 10000,
;;  "R21,R11,R22,R23,R12,W1,W2" => 1000,
;;  "R21,R11,R22,R23,R12,W2,W1" => 100,
;;  "R21,R11,R22,R23,W2,R12,W1" => 10000,
;;  "R21,R22,R11,R12,R23,W1,W2" => 1000,
;;  "R21,R22,R11,R12,R23,W2,W1" => 100,
;;  "R21,R22,R11,R12,W1,R23,W2" => 10000,
;;  "R21,R22,R11,R23,R12,W1,W2" => 1000,
;;  "R21,R22,R11,R23,R12,W2,W1" => 100,
;;  "R21,R22,R11,R23,W2,R12,W1" => 10000,
;;  "R21,R22,R23,R11,R12,W1,W2" => 1000,
;;  "R21,R22,R23,R11,R12,W2,W1" => 100,
;;  "R21,R22,R23,R11,W2,R12,W1" => 10000,
;;  "R21,R22,R23,W2,R11,R12,W1" => 1000000,
;; }

;; Which of these possibilities remain if we instead use serialized
;; procedures:
;;
;;      (define x 10)
;;
;;      (define s (make-serializer))
;;
;;      (parallel-execute (s (lambda () (set! x (* x x))))
;;                        (s (lambda () (set! x (* x x x)))))

;; just 2 of them:
;;
;;  "R11,R12,W1,R21,R22,R23,W2" => 1000000
;;  "R21,R22,R23,W2,R11,R12,W1" => 1000000,
