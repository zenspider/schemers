;; (put 'scheme-define 'scheme-indent-function 1)

(module eval

  (eval add-binding-to-frame!
        application?
        apply
        apply-primitive-procedure
        assignment-value
        assignment-variable
        assignment?
        begin-actions
        begin?
        compound-procedure?
        cond->if
        cond-actions
        cond-clauses
        cond-else-clause?
        cond-predicate
        cond?
        define-variable!
        definition-value
        definition-variable
        definition?
        enclosing-environment
        eval-assignment
        eval-definition
        eval-if
        eval-sequence
        expand-clauses
        extend-environment
        false
        first-exp
        first-frame
        first-operand
        frame-values
        frame-variables
        if-alternative
        if-consequent
        if-predicate
        if?
        lambda-body
        lambda-parameters
        lambda?
        last-exp?
        list-of-values
        lookup-variable-value
        make-begin
        make-frame
        make-if
        make-lambda
        make-procedure
        no-operands?
        null
        operands
        operator
        primitive-procedure?
        procedure-body
        procedure-environment
        procedure-parameters
        quoted?
        rest-exps
        rest-frames
        rest-operands
        self-evaluating?
        sequence->exp
        set-variable-value!
        set-variable-value!
        tagged-list?
        text-of-quotation
        the-empty-environment
        true?
        variable?)

  (import

   ;; (import (prefix (only scheme define) foo-))

   (only scheme
         caadr cadddr caddr cadr car cdadr cdddr cddr cdr cond cons
         define eq? if let list not null? number? or pair? quote
         string? symbol?
         < length = set-cdr! set-car!)

   (only chicken error))

  (define (eval exp env)
    (cond ((self-evaluating? exp) exp)
          ((variable?        exp) (lookup-variable-value exp env))
          ((quoted?          exp) (text-of-quotation exp))
          ((assignment?      exp) (eval-assignment exp env))
          ((definition?      exp) (eval-definition exp env))
          ((if?              exp) (eval-if exp env))
          ((lambda?          exp) (make-procedure (lambda-parameters exp)
                                                  (lambda-body exp) env))
          ((begin?           exp) (eval-sequence (begin-actions exp) env))
          ((cond?            exp) (eval (cond->if exp) env))
          ((application?     exp) (apply (eval (operator exp) env)
                                         (list-of-values (operands exp) env)))
          (else
           (error "Unknown expression type -- EVAL" exp))))

  (define (apply procedure arguments)
    (cond ((primitive-procedure? procedure)
           (apply-primitive-procedure procedure arguments))
          ((compound-procedure? procedure)
           (eval-sequence (procedure-body procedure)
                          (extend-environment (procedure-parameters procedure)
                                              arguments
                                              (procedure-environment procedure))))
          (else
           (error "Unknown procedure type -- APPLY" procedure))))

  ;; Simple Aliases:

  (define application?        pair?)
  (define assignment-value    caddr)
  (define assignment-variable cadr)
  (define begin-actions       cdr)
  (define cond-actions        cdr)
  (define cond-clauses        cdr)
  (define cond-predicate      car)
  (define enclosing-environment cdr)
  (define false               #f)
  (define first-exp           car)
  (define first-frame           car)
  (define first-operand       car)
  (define frame-values          cdr)
  (define frame-variables       car)
  (define if-consequent       caddr)
  (define if-predicate        cadr)
  (define lambda-body         cddr)
  (define lambda-parameters   cadr)
  (define no-operands?        null?)
  (define null                '())
  (define operands            cdr)
  (define operator            car)
  (define procedure-body        caddr)
  (define procedure-environment cadddr)
  (define procedure-parameters  cadr)
  (define rest-exps           cdr)
  (define rest-frames         cdr)
  (define rest-operands       cdr)
  (define text-of-quotation   cadr)
  (define the-empty-environment null)

  ;; Support Functions (sorted):

  (define (assignment? exp)
    (tagged-list? exp 'set))

  (define (begin? exp)
    (tagged-list? exp 'begin))

  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

  (define (cond-else-clause? clause)
    (eq? (cond-predicate clause) 'else))

  (define (cond? exp)
    (tagged-list? exp 'cond))

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

  (define (eval-assignment exp env)
    (set-variable-value! (assignment-variable exp)
                         (eval (assignment-value exp) env)
                         env)
    'ok)

  (define (eval-definition exp env)
    (define-variable! (definition-variable exp)
      (eval (definition-value exp) env)
      env)
    'ok)

  (define (eval-if exp env)
    (if (true? (eval (if-predicate exp) env))
        (eval (if-consequent exp) env)
        (eval (if-alternative exp) env)))

  (define (eval-sequence exps env)
    (cond ((last-exp? exps) (eval (first-exp exps) env))
          (else (eval (first-exp exps) env)
                (eval-sequence (rest-exps exps) env))))

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

  (define (false? exp)
    (eq? exp false))

  (define (make-procedure params body env)
    (list 'proc params body env))

  (define (compound-procedure? exp)
    (tagged-list? exp 'proc))

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

  (define (list-of-values exps env)
    (if (no-operands? exps) null
        (cons (eval (first-operand exps) env)
              (list-of-values (rest-operands exps) env))))

  (define (make-begin seq)
    (cons 'begin seq))

  (define (make-if predicate consequent alternative)
    (if alternative
        (list 'if predicate consequent alternative)
        (list 'if predicate consequent)))

  (define (make-lambda parameters body)
    (cons 'lambda (cons parameters body))) ;; TODO: list?

  (define (quoted? exp)
    (tagged-list? exp 'quote))

  (define (self-evaluating? exp)
    (or (number? exp) (string? exp)))

  (define (sequence->exp seq)
    (cond ((null? seq) seq)
          ((last-exp? seq)
           (first-exp seq))
          (else (make-begin seq))))

  (define (tagged-list? exp tag)
    (if (pair? exp)
        (eq? (car exp) tag)
        false))

  (define (true? exp)
    (not (eq? exp false)))

  (define (variable? exp) (symbol? exp))

  (define (make-frame vars vals)
    (cons vars vals))

  (define (add-binding-to-frame! var val frame)
    (set-car! frame (cons var (car frame)))
    (set-cdr! frame (cons val (cdr frame))))

  (define (extend-environment vars vals base-env)
    (if (= (length vars) (length vals))
        (cons (make-frame vars vals) base-env)
        (if (< (length vars) (length vals))
            (error "Too many arguments supplied" vars vals)
            (error "Too few arguments supplied"  vars vals))))

  (define (lookup-variable-value var env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (car vals))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (set-variable-value! var val env)
    (define (env-loop env)
      (define (scan vars vals)
        (cond ((null? vars)
               (env-loop (enclosing-environment env)))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (if (eq? env the-empty-environment)
          (error "Unbound variable -- SET!" var)
          (let ((frame (first-frame env)))
            (scan (frame-variables frame)
                  (frame-values frame)))))
    (env-loop env))

  (define (define-variable! var val env)
    (let ((frame (first-frame env)))
      (define (scan vars vals)
        (cond ((null? vars)
               (add-binding-to-frame! var val frame))
              ((eq? var (car vars))
               (set-car! vals val))
              (else (scan (cdr vars) (cdr vals)))))
      (scan (frame-variables frame)
            (frame-values frame))))

  ;; Undefined Expressions

  (define (apply-primitive-procedure proc args) null)
  (define (primitive-procedure?      exp) null)
  )

