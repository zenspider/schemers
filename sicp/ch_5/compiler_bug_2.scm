#!/usr/bin/env csi -s

(require rackunit)
(require-library compiler)
(import compiler)

(assert-compile 7 '(+ 1 (* 2 3)))

(assert-assemble
 7
 '((assign arg1 (const 1))
   (save arg1)
   (assign arg1 (const 2))
   (assign arg2 (const 3))
   (assign arg2 (op *) (reg arg1) (reg arg2))
   (restore arg1)
   (assign val (op +) (reg arg1) (reg arg2))))

(assert-compile 11 '(+ 1 (* 2 3) 4))

(assert-assemble
 11
 '((assign arg1 (const 1))
   (save arg1)
   (assign arg1 (const 2))
   (assign arg2 (const 3))
   (assign arg2 (op *) (reg arg1) (reg arg2))
   (restore arg1)
   (assign arg1 (op +) (reg arg1) (reg arg2))
   (assign arg2 (const 4))
   (assign val (op +) (reg arg1) (reg arg2))))

(test '((assign arg1 (const 1))
        (save arg1)
        (assign arg1 (const 2))
        (assign arg2 (const 3))
        (assign arg2 (op *) (reg arg1) (reg arg2))
        (restore arg1)
        (assign arg1 (op +) (reg arg1) (reg arg2))
        (assign arg2 (const 4))
        (assign val (op +) (reg arg1) (reg arg2)))
      (statements (compile '(+ 1 (* 2 3) 4) 'val 'next)))


