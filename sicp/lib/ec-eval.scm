;; (put 'scheme-define 'scheme-indent-function 1)

(require-library machine)

(module ec-eval
  *

  (import

   (only scheme
         * + - / < = > and append caadr caar cadddr caddr cadr car
         cdadr cdddr cddr cdr cond cons define eof-object? eq? if lambda length
         let list map not null? number? or pair? quote set! set-car! set-cdr!
         string? symbol? read display newline)

   (prefix (only scheme apply) scheme-) ; scheme-apply

   (only chicken error use)

   machine)

  (use extras)
  (use (only srfi-1 zip))

  (define (empty-arglist) '())

  (define (adjoin-arg arg arglist)
    (append arglist (list arg)))

  (define (last-operand? ops)
    (null? (cdr ops)))

  (define (eof? exp)
    (eof-object? exp))

  (define (prompt-for-input string)
    (newline) (newline) (display string) (newline))

  (define (announce-output string)
    (newline) (display string) (newline))

  (define (user-print obj)
    (if (compound-procedure? obj)
        (display (list 'compound-procedure
                       (procedure-params obj)
                       (procedure-body obj)
                       '<procedure-env>))
        (display obj)))

  (define (get-global-environment)
    the-global-environment)

  ;; Simple Aliases:

  (define (cdar x) (cdr (car x)))

  (define application?             pair?)
  (define assignment-value         caddr)
  (define assignment-variable      cadr)
  (define begin-actions            cdr)
  (define compiled-procedure-entry cadr)
  (define compiled-procedure-env   caddr)
  (define cond-actions             cdr)
  (define cond-clauses             cdr)
  (define cond-predicate           car)
  (define cond-rest                cdr)
  (define false                    #f)
  (define first-exp                car)
  (define first-frame              car)
  (define first-operand            car)
  (define if-consequent            caddr)
  (define if-predicate             cadr)
  (define lambda-body              cddr)
  (define lambda-params            cadr)
  (define let-body                 cddr)
  (define let-params               cadr)
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
    (list (list '*      *)
          (list '+      +)
          (list '-      -)
          (list '/      /)
          (list '=      =)
          (list '>      >)
          (list 'car    car)
          (list 'cdr    cdr)
          (list 'cons   cons)
          (list 'eq?    eq?)
          (list 'length length)
          (list 'null?  null?)))

  ;; Support Functions (sorted):

  (define (add-binding-to-frame! var val frame)
    (set-cdr! frame (cons (car frame) (cdr frame)))
    (set-car! frame (list var val)))

  (define (apply-primitive-procedure proc args)
    (scheme-apply (primitive-implementation proc) args))

  (define (assignment? exp)
    (tagged-list? exp 'set))

  (define (begin? exp)
    (tagged-list? exp 'begin))

  (define (compiled-procedure? proc)
    (tagged-list? proc 'compiled-procedure))

  (define (compound-procedure? exp)
    (tagged-list? exp 'proc))

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
    (cond ((null? frame)            null)
          ((eq? var (caar frame))   (car frame))
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

  (define (let-args exp)
    (map car (let-params exp)))

  (define (let-vals exp)
    (map cadr (let-params exp)))

  (define (let->call args vals body)
    (append (list (append (list 'lambda args) body)) vals))

  (define (let->combination exp)
    (let->call (let-args exp) (let-vals exp) (let-body exp)))

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

  (define (cond->if exp)
    (expand-clauses (cond-clauses exp)))

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


  (define (true? exp)
    (not (eq? exp false)))

  (define (variable? exp) (symbol? exp))

  (define (unassigned? exp)
    (eq? '*unassigned* exp))

  (define (debug-exp label exp)
    (printf "~s = ~s~n" label exp)
    #f)

  (define (false? exp)
    (eq? exp false))

  (define eceval-operations
    (list
     (list 'adjoin-arg                adjoin-arg)
     (list 'announce-output           announce-output)
     (list 'application?              application?)
     (list 'apply-primitive-procedure apply-primitive-procedure)
     (list 'assignment-value          assignment-value)
     (list 'assignment-variable       assignment-variable)
     (list 'assignment?               assignment?)
     (list 'begin-actions             begin-actions)
     (list 'begin?                    begin?)
     (list 'car                       car)
     (list 'cdr                       cdr)
     (list 'compiled-procedure-entry  compiled-procedure-entry)
     (list 'compiled-procedure-env    compiled-procedure-env)
     (list 'compiled-procedure?       compiled-procedure?)
     (list 'compound-procedure?       compound-procedure?)
     (list 'cond-actions              cond-actions)
     (list 'cond-clauses              cond-clauses)
     (list 'cond-predicate            cond-predicate)
     (list 'cond-rest                 cond-rest)
     (list 'cond?                     cond?)
     (list 'cons                      cons)
     (list 'debug-exp                 debug-exp)
     (list 'define-variable!          define-variable!)
     (list 'definition-value          definition-value)
     (list 'definition-variable       definition-variable)
     (list 'definition?               definition?)
     (list 'empty-arglist             empty-arglist)
     (list 'eof?                      eof?)
     (list 'extend-environment        extend-environment)
     (list 'first-exp                 first-exp)
     (list 'first-operand             first-operand)
     (list 'get-global-environment    get-global-environment)
     (list 'if-alternative            if-alternative)
     (list 'if-consequent             if-consequent)
     (list 'if-predicate              if-predicate)
     (list 'if?                       if?)
     (list 'lambda-body               lambda-body)
     (list 'lambda-parameters         lambda-params)
     (list 'lambda?                   lambda?)
     (list 'last-exp?                 last-exp?)
     (list 'last-operand?             last-operand?)
     (list 'let->combination          let->combination)
     (list 'let?                      let?)
     (list 'list                      list)
     (list 'lookup-variable-value     lookup-variable-value)
     (list 'make-procedure            make-procedure)
     (list 'no-operands?              no-operands?)
     (list 'null?                     null?)
     (list 'operands                  operands)
     (list 'operator                  operator)
     (list 'primitive-procedure?      primitive-procedure?)
     (list 'procedure-body            procedure-body)
     (list 'procedure-environment     procedure-environment)
     (list 'procedure-parameters      procedure-params)
     (list 'prompt-for-input          prompt-for-input)
     (list 'quoted?                   quoted?)
     (list 'read                      read)
     (list 'rest-exps                 rest-exps)
     (list 'rest-operands             rest-operands)
     (list 'self-evaluating?          self-evaluating?)
     (list 'set-variable-value!       set-variable-value!)
     (list 'text-of-quotation         text-of-quotation)
     (list 'true?                     true?)
     (list 'unassigned?               unassigned?)
     (list 'user-print                user-print)
     (list 'variable?                 variable?)))

  ;; New Prelude:
  ;;
  ;; if flag is not set, start the repl
  ;; if exp is assigned, evaluate exp
  ;; if val is assigned, execute val

  (define ec-eval
    (make-machine
     '(exp env val proc argl continue unev arg1 arg2)
     eceval-operations
     '(compile-and-go

       (branch (label external-entry))       ; branches if `flag' is set
       (goto (label read-eval-print-loop))

       external-entry

       (perform (op initialize-stack))
       (assign env (op get-global-environment))
       (test (op unassigned?) (reg exp))
       (branch (label execute-external-entry))

       evaluate-external-entry

       ;; exp is already assigned
       (assign continue (label done))
       (goto (label eval-dispatch))

       execute-external-entry

       (assign continue (label print-result))
       (goto (reg val))

       read-eval-print-loop

       (perform (op initialize-stack))
       (perform (op prompt-for-input) (const ";;; EC-Eval Input:"))
       (assign exp (op read))
       (assign env (op get-global-environment))
       (assign continue (label print-result))
       (goto (label eval-dispatch))

       eval-dispatch

       ;; (perform (op debug-exp) (const eval-dispatch) (reg exp))

       (test (op self-evaluating?) (reg exp)) (branch (label ev-self-eval))
       (test (op variable?)        (reg exp)) (branch (label ev-variable))
       (test (op quoted?)          (reg exp)) (branch (label ev-quoted))
       (test (op assignment?)      (reg exp)) (branch (label ev-assignment))
       (test (op definition?)      (reg exp)) (branch (label ev-definition))
       (test (op if?)              (reg exp)) (branch (label ev-if))
       (test (op lambda?)          (reg exp)) (branch (label ev-lambda))
       (test (op begin?)           (reg exp)) (branch (label ev-begin))
       (test (op cond?)            (reg exp)) (branch (label ev-cond))
       (test (op let?)             (reg exp)) (branch (label ev-let))
       (test (op application?)     (reg exp)) (branch (label ev-application))
       (test (op eof?)             (reg exp)) (branch (label done))
       (goto (label unknown-expression-type))

       ev-self-eval

       (assign val (reg exp))
       (goto (reg continue))

       ev-variable

       (assign val (op lookup-variable-value) (reg exp) (reg env))
       (goto (reg continue))

       ev-quoted

       (assign val (op text-of-quotation) (reg exp))
       (goto (reg continue))

       ev-lambda

       (assign unev (op lambda-parameters) (reg exp))
       (assign exp (op lambda-body) (reg exp))
       (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
       (goto (reg continue))

       ev-let

       (assign exp (op let->combination) (reg exp))
       ;; fall through to ev-application

       ev-application

       (save continue)
       (save env)
       (assign unev (op operands) (reg exp))
       (save unev)                    ; wtf?
       (assign exp (op operator) (reg exp))
       (assign continue (label ev-appl-did-operator))
       (goto (label eval-dispatch))

       ev-appl-did-operator

       (restore unev)                 ; the operands
       (restore env)
       (assign argl (op empty-arglist))
       (assign proc (reg val))        ; the operator
       (test (op no-operands?) (reg unev))
       (branch (label apply-dispatch))
       (save proc)

       ev-appl-operand-loop

       (save argl)
       (assign exp (op first-operand) (reg unev))
       (test (op last-operand?) (reg unev))
       (branch (label ev-appl-last-arg))
       (save env)
       (save unev)
       (assign continue (label ev-appl-accumulate-arg))
       (goto (label eval-dispatch))

       ev-appl-accumulate-arg

       (restore unev)
       (restore env)
       (restore argl)
       (assign argl (op adjoin-arg) (reg val) (reg argl))
       (assign unev (op rest-operands) (reg unev))
       (goto (label ev-appl-operand-loop))

       ev-appl-last-arg

       (assign continue (label ev-appl-accum-last-arg))
       (goto (label eval-dispatch))

       ev-appl-accum-last-arg

       (restore argl)
       (assign argl (op adjoin-arg) (reg val) (reg argl))
       (restore proc)
       (goto (label apply-dispatch))

       apply-dispatch

       (test (op primitive-procedure?) (reg proc))
       (branch (label primitive-apply))
       (test (op compound-procedure?) (reg proc))
       (branch (label compound-apply))
       (test (op compiled-procedure?) (reg proc))
       (branch (label compiled-apply))
       (goto (label unknown-procedure-type))

       primitive-apply

       (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
       (restore continue)
       (goto (reg continue))

       compound-apply

       (assign unev (op procedure-parameters)  (reg proc))
       (assign env  (op procedure-environment) (reg proc))
       (assign env  (op extend-environment)    (reg unev) (reg argl) (reg env))
       (assign unev (op procedure-body)        (reg proc))
       (goto (label ev-sequence))

       compiled-apply

       (restore continue)
       (assign val (op compiled-procedure-entry) (reg proc))
       (goto (reg val))

       ev-begin

       (assign unev (op begin-actions) (reg exp))
       (save continue)
       (goto (label ev-sequence))

       ev-sequence

       (assign exp (op first-exp) (reg unev))
       (test (op last-exp?) (reg unev))
       (branch (label ev-sequence-last-exp))
       (save unev)
       (save env)
       (assign continue (label ev-sequence-continue))
       (goto (label eval-dispatch))

       ev-sequence-continue

       (restore env)
       (restore unev)
       (assign unev (op rest-exps) (reg unev))
       (goto (label ev-sequence))

       ev-sequence-last-exp

       (restore continue)
       (goto (label eval-dispatch))

       ev-cond

       ;; skip over 'cond'
       (assign exp (op cond-clauses) (reg exp))
       (save exp)
       (save env)
       (save continue)
       (assign continue (label ev-cond-decide))
       (assign exp (op cond-predicate) (reg exp))
       ;; haha cheating! FIX: I think this isn't a push/pop :(
       (perform (op define-variable!) (const else) (const true) (reg env))
       (goto (label eval-dispatch))

       ev-cond-decide

       (restore continue)
       (restore env)
       (restore exp)
       (test (op true?) (reg val))
       (branch (label ev-cond-true))
       (assign exp (op cond-rest) (reg exp))
       (test (op null?) (reg exp))
       (branch (label ev-cond-done))
       (save exp)
       (save env)
       (save continue)
       (assign continue (label ev-cond-decide))
       (assign exp (op cond-predicate) (reg exp))
       (goto (label eval-dispatch))

       ev-cond-true

       (assign exp (op cond-actions) (reg exp))
       (assign unev (reg exp))
       (save continue)
       (goto (label ev-sequence))

       ev-cond-done

       (assign val (const *undefined*))
       (goto (reg continue))

       ev-if

       (save exp)
       (save env)
       (save continue)
       (assign continue (label ev-if-decide))
       (assign exp (op if-predicate) (reg exp))
       (goto (label eval-dispatch))

       ev-if-decide

       (restore continue)
       (restore env)
       (restore exp)
       (test (op true?) (reg val))
       (branch (label ev-if-consequent))

       ev-if-alternative

       (assign exp (op if-alternative) (reg exp))
       (goto (label eval-dispatch))

       ev-if-consequent

       (assign exp (op if-consequent) (reg exp))
       (goto (label eval-dispatch))

       ev-assignment

       (assign unev (op assignment-variable) (reg exp))
       (save unev)                    ; save variable for later
       (assign exp (op assignment-value) (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-assignment-1))
       (goto (label eval-dispatch))  ; evaluate the assignment value

       ev-assignment-1

       (restore continue)
       (restore env)
       (restore unev)
       (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))

       ev-definition

       (assign unev (op definition-variable) (reg exp))
       (save unev)                    ; save variable for later
       (assign exp (op definition-value) (reg exp))
       (save env)
       (save continue)
       (assign continue (label ev-definition-1))
       (goto (label eval-dispatch))

       ev-definition-1

       (restore continue)
       (restore env)
       (restore unev)
       (perform (op define-variable!) (reg unev) (reg val) (reg env))
       (assign val (const ok))
       (goto (reg continue))

       print-result

       (perform (op announce-output) (const ";;; EC-Eval Value:"))
       (perform (op user-print) (reg val))
       (goto (label read-eval-print-loop))

       unknown-expression-type

       (assign val (const unknown-expression-type-error))
       (perform (op user-print) (reg exp))
       (goto (label signal-error))

       unknown-procedure-type

       (restore continue)                 ; clean up stack (from apply-dispatch)
       (assign val (const unknown-procedure-type-error))
       (goto (label signal-error))

       signal-error

       (perform (op user-print) (reg val))
       (perform (op announce-output) (const "Bye!"))
       (goto (label done))

       done

       )))

  (define the-global-environment
    (setup-environment)))
