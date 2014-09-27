;;;
;; Instruction Summary:
;;
;; (assign <register-name> (const <value))
;; (assign <register-name> (label <name>))
;; (assign <register-name> (op <name>) <input> ...)
;; (assign <register-name> (reg <register-name>))
;; (branch (label <name>))
;; (goto (label <name>))
;; (goto (reg <name>))
;; (perform (op <name) <input> ...)
;; (restore <name>)
;; (save <name>)
;; (test (op <name>) <input> ...)

(module machine
  *

  (import scheme chicken test data-structures)

  (use extras)

;;; Top-level Machine Functions:

    (define (make-machine register-names operations controller-text)
      (let ((machine (make-new-machine)))
        (for-each (lambda (register-name)
                    ((machine 'allocate-register) register-name))
                  register-names)
        ((machine 'install-operations) operations)
        ((machine 'install-instruction-sequence)
         (assemble controller-text machine))
        machine))

    (define (set-register-contents! machine register-name value)
      (set-contents! (get-register machine register-name) value)
      'done)

    (define (get-register-contents machine register-name)
      (get-contents (get-register machine register-name)))

    (define (start machine)
      (machine 'start))

    (define (trace-on machine)
      (machine 'trace-on))

    (define (trace-off machine)
      (machine 'trace-off))

    (define (trace-register-on machine name)
      (((machine 'get-register) name) 'trace-on))

    (define (trace-register-off machine name)
      (((machine 'get-register) name) 'trace-off))

    (define (append-operations machine . operations)
      ((machine 'install-operations) (chop operations 2)))

    (define-syntax assert-machine
      (syntax-rules ()
        ((_ machine inputs output expected)
         (let* ((args (map car inputs))
                (call (append (list (quote machine)) args))
                (desc (sprintf "(assert-machine ~s ~s)" expected call)))
           (for-each (lambda (in)
                       (let ((key (car in)) (val (cadr in)))
                         (set-register-contents! machine key val)))
                     inputs)
           (set-register-contents! ec-eval 'flag true) ; outside control
           (start machine)
           (test desc expected (get-register-contents machine output))))))

;;; Stupid Aliases:

    (define assign-reg-name                 cadr)
    (define assign-value-exp                cddr)
    (define branch-dest                     cadr)
    (define constant-exp-value              cadr)
    (define goto-dest                       cadr)
    (define instruction-execution-proc      cdr)
    (define instruction-text                car)
    (define label-exp-label                 cadr)
    (define label?                          symbol?)
    (define make-label-entry                cons)
    (define (operation-exp-op x) (cadr (car x))) ;; cadar == (cadr (car exp)) ?
    (define operation-exp-operands          cdr)
    (define perform-action                  cdr)
    (define register-exp-reg                cadr)
    (define set-instruction-execution-proc! set-cdr!)
    (define stack-inst-reg-name             cadr)
    (define test-condition                  cdr)

;;; Type functions

    (define (tagged-list? exp tag)
      (and (pair? exp) (eq? (car exp) tag)))

    (define (constant-exp?  exp) (tagged-list? exp 'const))

    (define (label-exp?     exp) (tagged-list? exp 'label))

    (define (operation-exp? exp)
      (and (pair? exp) (tagged-list? (car exp) 'op)))

    (define (register-exp?  exp) (tagged-list? exp 'reg))

;;; Support Functions:

    (define (advance-pc pc)
      (set-contents! pc (cdr (get-contents pc))))

    (define (assemble controller-text machine)
      (extract-labels controller-text
                      (lambda (insts labels)
                        (update-insts! insts labels machine)
                        insts)))

    (define (extract-labels text receive)
      (if (null? text)
          (receive '() '())
          (extract-labels (cdr text)
                          (lambda (insts labels)
                            (let ((next-inst (car text)))
                              (if (label? next-inst)
                                  (if (assoc next-inst labels)
                                      (error "Duplicate label: " next-inst)
                                      (receive insts
                                        (cons (make-label-entry next-inst insts)
                                              labels)))
                                  (receive (cons (make-instruction next-inst)
                                                 insts)
                                    labels)))))))

    (define (get-contents register)
      (register 'get))

    (define (get-register machine register-name)
      ((machine 'get-register) register-name))

    (define (lookup-label labels label-name)
      (let ((val (assoc label-name labels)))
        (if val
            (cdr val)
            (error "Undefined label -- ASSEMBLE" label-name))))

    (define (lookup-prim symbol operations)
      (let ((val (assoc symbol operations)))
        (if val
            (cadr val)
            (error "Undefined operation -- ASSEMBLE" symbol))))

    (define (make-assign inst machine labels operations pc)
      (let ((target    (get-register machine (assign-reg-name inst)))
            (value-exp (assign-value-exp inst)))
        (let ((value-proc
               (if (operation-exp? value-exp)
                   (make-operation-exp value-exp machine labels operations)
                   (make-primitive-exp (car value-exp) machine labels))))
          (lambda ()
            (set-contents! target (value-proc))
            (advance-pc pc)))))

    (define (make-branch  inst machine labels           flag pc)
      (let ((dest (branch-dest inst)))
        (if (label-exp? dest)
            (let ((insts (lookup-label labels (label-exp-label dest))))
              (lambda ()
                (if (get-contents flag)
                    (set-contents! pc insts)
                    (advance-pc pc))))
            (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

    (define (make-execution-procedure inst labels machine pc flag stack ops)
      (case (car inst)
        ((assign)  (make-assign  inst machine labels       ops      pc))
        ((test)    (make-test    inst machine labels       ops flag pc))
        ((branch)  (make-branch  inst machine labels           flag pc))
        ((goto)    (make-goto    inst machine labels                pc))
        ((save)    (make-save    inst machine        stack          pc))
        ((restore) (make-restore inst machine        stack          pc))
        ((perform) (make-perform inst machine labels       ops      pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

    (define (make-goto inst machine labels pc)
      (let ((dest (goto-dest inst)))
        (cond ((label-exp? dest)
               (let ((insts (lookup-label labels (label-exp-label dest))))
                 (lambda () (set-contents! pc insts))))
              ((register-exp? dest)
               (let ((reg (get-register machine (register-exp-reg dest))))
                 (lambda () (set-contents! pc (get-contents reg)))))
              (else (error "Bad GOTO instruction -- ASSEMBLE" inst)))))

    (define (make-instruction text)
      (cons text '()))

    (define (make-new-machine)
      (let ((pc (make-register 'pc))
            (flag (make-register 'flag))
            (stack (make-stack))
            (the-instruction-sequence '())
            (tracing #f)
            (instruction-count 0))

        ((flag 'set) #f)

        (let ((the-ops (list (list 'initialize-stack
                                   (lambda () (stack 'initialize)))
                             (list 'print-stack-statistics
                                   (lambda () (stack 'print-statistics)))))
              (register-table (list (list 'pc pc)
                                    (list 'flag flag))))

          (define (allocate-register name)
            (if (assoc name register-table)
                (error "Multiply defined register: " name)
                (set! register-table (cons (list name (make-register name))
                                           register-table)))
            'register-allocated)

          (define (lookup-register name)
            (let ((val (assoc name register-table)))
              (if val
                  (cadr val)
                  (error "Unknown register: " name))))

          (define (execute)
            (let ((insts (get-contents pc)))
              (set! instruction-count (+ 1 instruction-count))
              (if (null? insts)
                  'done
                  (begin
                    (if tracing
                        (printf "[~s] ~s~n" instruction-count (caar insts)))
                    ((instruction-execution-proc (car insts)))
                    (execute)))))

          (define (initialize)
            (stack 'initialize)
            (set! instruction-count 0)
            (set-contents! pc the-instruction-sequence))

          (define (get-statistics)
            (list (list 'instruction-count instruction-count)
                  (list 'stack (stack 'statistics))))

          (define (trace-on)
            (set! tracing #t))

          (define (trace-off)
            (set! tracing #f))

          (define (dispatch message)
            (case message
              ((start)
               (initialize)
               (execute))
              ((install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((allocate-register) allocate-register)
              ((get-register) lookup-register)
              ((install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((stack) stack)
              ((statistics) (get-statistics))
              ((trace-on)  (trace-on))
              ((trace-off) (trace-off))
              ((operations) the-ops)
              (else (error "Unknown request -- MACHINE" message))))

          dispatch)))

    (define (make-operation-exp exp machine labels operations)
      (let ((op (lookup-prim (operation-exp-op exp) operations))
            (aprocs (map (lambda (e)
                           (make-primitive-exp e machine labels))
                         (operation-exp-operands exp))))
        (lambda () (apply op (map (lambda (p) (p)) aprocs)))))

    (define (make-perform inst machine labels operations pc)
      (let ((action (perform-action inst)))
        (if (operation-exp? action)
            (let ((action-proc
                   (make-operation-exp action machine labels operations)))
              (lambda ()
                (action-proc)
                (advance-pc pc)))
            (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

    (define (make-primitive-exp exp machine labels)
      (cond ((constant-exp? exp)
             (let ((c (constant-exp-value exp)))
               (lambda () c)))
            ((label-exp? exp)
             (let ((insts (lookup-label labels (label-exp-label exp))))
               (lambda () insts)))
            ((register-exp? exp)
             (let ((r (get-register machine (register-exp-reg exp))))
               (lambda () (get-contents r))))
            (else (error "Unknown expression type --ASSEMBLE" exp))))

    (define (make-register name)
      (let ((contents '*unassigned*)
            (tracing #f))
        (define (dispatch message)
          (case message
            ((get) contents)
            ((set) (lambda (value)
                     (when tracing
                       (case name
                         ((exp) (printf "register: ~s = ~s~n" name value))
                         ((env)
                          (printf "env-set:~n")
                          (for-each (lambda (pair) (printf "  ~s = ~s~n"
                                                           (car pair)
                                                           (cadr pair)))
                                    (car value)))
                         (else  (printf "register: ~s = ...~n" name) )))
                     (set! contents value)))
            ((trace-on)  (set! tracing #t))
            ((trace-off) (set! tracing #f))
            (else  (error "Unknown request -- REGISTER" message))))
        dispatch))

    (define (make-restore inst machine stack pc)
      (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
          (set-contents! reg (pop stack))
          (advance-pc pc))))

    (define (make-save inst machine stack pc)
      (let ((reg (get-register machine (stack-inst-reg-name inst))))
        (lambda ()
          (push stack (get-contents reg))
          (advance-pc pc))))

    (define (make-stack)
      (let ((s '())
            (number-pushes 0)
            (max-depth 0)
            (current-depth 0))
        (define (push x)
          (set! s (cons x s))
          (set! number-pushes (+ 1 number-pushes))
          (set! current-depth (+ 1 current-depth))
          (set! max-depth (max current-depth max-depth)))
        (define (pop)
          (if (null? s)
              (error "Empty stack -- POP")
              (let ((top (car s)))
                (set! s (cdr s))
                (set! current-depth (- current-depth 1))
                top)))
        (define (initialize)
          (set! s '())
          (set! number-pushes 0)
          (set! max-depth 0)
          (set! current-depth 0))
        (define (print-statistics)
          (printf "pushes = ~s~Nmax-depth = ~s~N" number-pushes max-depth))
        (define (get-statistics)
          (list (list 'max-depth     max-depth)
                (list 'number-pushes number-pushes)))
        (define (dispatch message)
          (case message
            ((push)             push)
            ((pop)              (pop))
            ((initialize)       (initialize))
            ((print-statistics) (print-statistics))
            ((statistics)       (get-statistics))
            (else (error "Unknown request -- STACK" message))))
        dispatch))

    (define (make-test inst machine labels operations flag pc)
      (let ((condition (test-condition inst)))
        (if (operation-exp? condition)
            (let ((condition-proc (make-operation-exp
                                   condition machine labels operations)))
              (lambda ()
                (set-contents! flag (condition-proc))
                (advance-pc pc)))
            (error "Bad TEST instruction -- ASSEMBLE" inst))))

    (define (pop stack)
      (stack 'pop))

    (define (push stack value)
      ((stack 'push) value))

    (define (set-contents! register value)
      ((register 'set) value))

    (define (update-insts! insts labels machine)
      (let ((pc    (get-register machine 'pc))
            (flag  (get-register machine 'flag))
            (stack (machine 'stack))
            (ops   (machine 'operations)))
        (for-each
         (lambda (inst)
           (set-instruction-execution-proc! inst
                                            (make-execution-procedure
                                             (instruction-text inst)
                                             labels machine pc flag stack
                                             ops)))
         insts))))

