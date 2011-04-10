;; (put 'scheme-define 'scheme-indent-function 1)

(module amb-eval

  (eval eval
        setup-environment)

  (import

   (only scheme
         * + - / < = and append caadr caar cadddr caddr cadr car cdadr
         cdddr cddr cdr cond cons define eq? if lambda length let list
         map not null? number? or pair? quote set-car! set-cdr! string? set!
         symbol?)

   (prefix (only scheme apply) scheme-) ; scheme-apply

   (only chicken error use))

  (use (only extras printf))
  (use (only srfi-1 zip))

  (define (eval exp env)
    ((analyze exp) env))

  (define (analyze exp)
    (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
          ((quoted?          exp) (analyze-quoted          exp))
          ((variable?        exp) (analyze-variable        exp))
          ((assignment?      exp) (analyze-assignment      exp))
          ((definition?      exp) (analyze-definition      exp))
          ((if?              exp) (analyze-if              exp))
          ((lambda?          exp) (analyze-lambda          exp))
          ((begin?           exp) (analyze-sequence        exp))
          ((cond?            exp) (analyze                 (cond->if exp)))
          ((let?             exp) (analyze         (let->combination exp)))
          ((application?     exp) (analyze-application     exp))
          ((eq? #!eof exp) '*done*)
          (else
           (error "Unknown expression type -- EVAL" exp))))

  ;; Simple Aliases:

  (define application?             pair?)
  (define assignment-value         caddr)
  (define assignment-variable      cadr)
  (define cond-actions             cdr)
  (define cond-clauses             cdr)
  (define cond-predicate           car)
  (define false                    #f)
  (define first-exp                car)
  (define first-frame              car)
  (define first-operand            car)
  (define if-consequent            caddr)
  (define if-predicate             cadr)
  (define lambda-body              cddr)
  (define lambda-params            cadr)
  (define no-operands?             null?)
  (define null                     '())
  (define operands                 cdr)
  (define operator                 car)
  (define primitive-implementation cadr)
  (define procedure-body           caddr)
  (define procedure-environment    cadddr)
  (define procedure-params         cadr)
  (define rest-exps                cdr)
  (define rest-frames              cdr)
  (define rest-operands            cdr)
  (define text-of-quotation        cadr)
  (define the-empty-environment    null)
  (define true                     #t)

  ;; Other Values:

  (define primitive-procedures
    (list (list '*     *)
          (list '+     +)
          (list '-     -)
          (list '/     /)
          (list '=     =)
          (list 'car   car)
          (list 'cdr   cdr)
          (list 'cons  cons)
          (list 'null? null?)))

  ;; Support Functions (sorted):

  (define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cons (car frame) (cdr frame)))
    (set-car! frame (list var val)))

  (define (analyze-application exp)
    (let ((fproc (analyze (operator exp)))
          (aprocs (map analyze (operands exp))))
      (lambda (env)
        (execute-application (fproc env)
                             (map (lambda (aproc) (aproc env)) aprocs)))))

  (define (analyze-assignment exp)
    (let ((var (assignment-variable exp))
          (vproc (analyze (assignment-value exp))))
      (lambda (env)
        (set-variable-value! var (vproc env) env)
        'ok)))

  (define (analyze-definition exp)
    (let ((var (definition-variable exp))
          (vproc (analyze (definition-value exp))))
      (lambda (env)
        (define-variable! var (vproc env) env)
        'ok)))

  (define (analyze-if exp)
    (let ((pproc (analyze (if-predicate exp)))
          (cproc (analyze (if-consequent exp)))
          (aproc (analyze (if-alternative exp))))
      (lambda (env)
        (if (true? (pproc env))
            (cproc env)
            (aproc env)))))

  (define (analyze-lambda exp)
    (let ((vars (lambda-params exp))
          (bproc (analyze-sequence (lambda-body exp))))
      (lambda (env) (make-procedure vars bproc env))))

  (define (analyze-quoted exp)
    (let ((val (text-of-quotation exp)))
      (lambda (env) val)))

  (define (analyze-self-evaluating exp)
    (lambda (env) exp))

  (define (analyze-sequence exps)
    (define (sequentially p1 p2)
      (lambda (env) (p1 env) (p2 env)))
    (define (loop p1 p*)
      (if (null? p*) p1
          (loop (sequentially p1 (car p*)) (cdr p*))))
    (let ((procs (map analyze exps)))
      (if (null? procs)
          (error "Empty sequence -- ANALYZE"))
      (loop (car procs) (cdr procs))))

  (define (analyze-variable exp)
    (lambda (env) (lookup-variable-value exp env)))

  (define let-params cadr)
  (define let-body   cddr)

  (define (let-args exp)
    (map car (let-params exp)))

  (define (let-vals exp)
    (map cadr (let-params exp)))

  (define (let->call args vals body)
    (append (list (append (list 'lambda args) body)) vals))

  (define (let->combination exp)
    (let->call (let-args exp) (let-vals exp) (let-body exp)))

  (define (apply-primitive-procedure proc args)
    (scheme-apply (primitive-implementation proc) args))

  (define (assignment? exp)
    (tagged-list? exp 'set))

  (define (begin? exp)
    (tagged-list? exp 'begin))

  (define (compound-procedure? exp)
    (tagged-list? exp 'proc))

  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

  (define (cond? exp)
    (tagged-list? exp 'cond))

  (define (define-variable! var val env)
    (let ((pair (find-pair-in-frame (first-frame env) var)))
      (if (null? pair)
          (add-binding-to-frame! var val (first-frame env))
          (set-cdr! pair (list val)))))

  (define (definition-value exp)
    (if (symbol? (cadr exp))
        (caddr exp)
        (make-lambda (cdadr exp)        ; params
                     (cddr exp))))      ; body

  (define (definition-variable exp)
    (if (symbol? (cadr exp))
        (cadr exp)
        (caadr exp)))

  (define (definition? exp)
    (tagged-list? exp 'define))

  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

  (define (execute-application proc args)
    (cond ((primitive-procedure? proc)
           (apply-primitive-procedure proc args))
          ((compound-procedure? proc)
           ((procedure-body proc)
            (extend-environment (procedure-params proc)
                                args
                                (procedure-environment proc))))
          (else
           (error "Unknown procedure type -- APPLY" proc))))

  (define (expand-clauses clauses)
    (if (null? clauses) 'false
        (let ((first (car clauses))
              (rest  (cdr clauses)))
          (if (cond-else-clause? first)
              (if (null? rest)
                  (sequence->exp (cond-actions first))
                  (error "ELSE clause isn't last --COND->IF" clauses))
              (make-if (cond-predicate first)
                       (sequence->exp (cond-actions first))
                       (expand-clauses rest))))))

  (define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied"  vars vals))))

  (define (find-pair-in-env env var)
    (if (eq? env the-empty-environment) null
        (let ((pair (find-pair-in-frame (first-frame env) var)))
          (if (null? pair) (find-pair-in-env (rest-frames env) var)
              pair))))

  (define (find-pair-in-frame frame var)
    (cond ((null? frame)          null)
          ((eq? var (caar frame)) (car frame))
          (else (find-pair-in-frame (cdr frame) var))))

  (define (if-alternative exp)
    (if (not (null? (cdddr exp)))
        (cadddr exp)
        'false))

  (define (if? exp)
    (tagged-list? exp 'if))

  (define (lambda? exp)
    (tagged-list? exp 'lambda))

  (define (last-exp? seq)
    (null? (cdr seq)))

  (define (let? exp)
    (tagged-list? exp 'let))

  (define (list-of-values exps env)
    (if (no-operands? exps) null
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

  (define (lookup-variable-value var env)
    (let ((pair (find-pair-in-env env var)))
      (if (null? pair)
          (error "Unbound variable" var)
          (cadr pair))))

  (define (make-begin seq)
    (cons 'begin seq))

  (define (make-frame vars vals)
    (zip vars vals))

  (define (make-if predicate consequent alternative)
    (if alternative
        (list 'if predicate consequent alternative)
        (list 'if predicate consequent)))

  (define (make-lambda params body)
    (append (list 'lambda params) body))

  (define (make-procedure params body env)
    (list 'proc params body env))

  (define (primitive-procedure-names)   (map car primitive-procedures))

  (define (primitive-procedure-objects)
    (map (lambda (x) (list 'prim (cadr x))) primitive-procedures))

  (define (primitive-procedure? exp)
    (tagged-list? exp 'prim))

  (define (quoted? exp)
    (tagged-list? exp 'quote))

  (define (self-evaluating? exp)
    (or (number? exp) (string? exp)))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq)
           (first-exp seq))
          (else (make-begin seq))))

  (define (set-variable-value! var val env)
    (let ((pair (find-pair-in-env env var)))
      (if (null? pair)
          (error "Unbound variable -- SET!" var)
          (set-cdr! pair (list val)))))

  (define (setup-environment)
    (let ((initial-env (extend-environment (primitive-procedure-names)
                                           (primitive-procedure-objects)
                                           the-empty-environment)))
      (define-variable! 'true true initial-env)
      (define-variable! 'false false initial-env)
      initial-env))

  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

  (define (true? exp)
    (not (eq? exp false)))

  (define (variable? exp) (symbol? exp))

  )
