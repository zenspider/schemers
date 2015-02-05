#lang racket

;; ;; (define-syntax block
;; ;;   (syntax-rules ()
;; ;;     ((_ label . body)
;; ;;      (let ((label (list 'label)))
;; ;;        (catch label . body)))))
;; ;;
;; ;; (define-syntax return-from
;; ;;   (syntax-rules ()
;; ;;     ((_ label value)
;; ;;      (throw label value))))
;;
;; ;;; 3.2.2 The Interpreter for Continuations
;;
;; (define (evaluate e r k)
;;   (if (not (pair? e))
;;       (cond ((symbol? e) (evaluate-variable e r k))
;;             (else        (evaluate-quote    e r k)))
;;       (case (car e)
;;         ((quote)  (evaluate-quote       (cadr e)                      r k))
;;         ((if)     (evaluate-if          (cadr e) (caddr e) (cadddr e) r k))
;;         ((begin)  (evaluate-begin       (cdr  e)                      r k))
;;         ((set!)   (evaluate-set!        (cadr e) (caddr e)            r k))
;;         ((lambda) (evaluate-lambda      (cadr e) (cddr  e)            r k))
;;         (else     (evaluate-application (car  e) (cdr   e)            r k)))))
;;
;; (define-generic (invoke (f) v* r k)
;;   (wrong "not a function" f r k))
;; (define-generic (resume (k continuation) v)
;;   (wrong "Unknown continuation" k))
;; (define-generic (lookup (r environment) n k)
;;   (wrong "not an environment" r n k))
;; (define-generic (update! (r environment) n k v)
;;   (wrong "not an environment" r n k))
;;
;; (define-class value        Object       ())
;; (define-class environment  Object       ())
;; (define-class continuation Object       (k))
;;
;; ;;; 3.2.3 Quoting
;;
;; (define (evaluate-quote v r k)
;;   (resume k v))
;;
;; ;;; 3.2.4 Alternatives
;;
;; (define-class if-cont      continuation (et ef r))
;;
;; (define (evaluate-if ec et ef r k)
;;   (evaluate ec r (make-if-cont k et ef r)))
;;
;; (define-method (resume (k if-cont) v)
;;   (evaluate (if v (if-cont-et k) (if-cont-ef k))
;;             (if-cont-r k)
;;             (if-cont-k k)))
;;
;; ;;; 3.2.5 Sequence
;;
;; (define-class begin-cont continuation (e* r))
;;
;; (define (evaluate-begin e* r k)
;;   (if (pair? e*)
;;       (if (pair? (cdr e*))
;;           (evaluate (car e*) r (make-begin-cont k e* r))
;;           (evaluate (car e*) r k))
;;       (resume k empty-begin-value)))
;;
;; (define-method (resume (k begin-cont) v)
;;   (evaluate-begin (cdr (begin-cont-e* k)) (begin-cont-r k) (begin-cont-k k)))
;;
;; ;;; 3.2.6 Variable Environment
;;
;; (define-class null-env     environment ())
;; (define-class full-env     environment (others name))
;; (define-class variable-env full-env    (value))
;;
;; (define (evaluate-variable n r k)
;;   (lookup r n k))
;;
;; (define-method (lookup (r null-env) n k)
;;   (wrong "Unknown variable" n r k))
;;
;; (define-method (lookup (r full-env) n k)
;;   (lookup (full-env-others r) n k))
;;
;; (define-method (lookup (r variable-env) n k)
;;   (if (eqv? n (variable-env-name r))
;;       (resume k (variable-env-value r))
;;       (lookup (variable-env-others r) n k)))
;;
;; (define-class set!-cont continuation (n r))
;;
;; (define (evaluate-set! n e r k)
;;   (evaluate e r (make-set!-cont k n r)))
;;
;; (define-method (resume (k set!-cont) v)
;;   (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v))
;;
;; (define-method (update! (r null-env) n k v)
;;   (wrong "Unknown variable" n r k))
;;
;; (define-method (update! (r full-env) n k v)
;;   (update! (full-env-others r) n k v))
;;
;; (define-method (update! (r variable-env) n k v)
;;   (if (eqv? n (variable-env-name r))
;;       (begin (set-variable-env-value! r v)
;;              (resume k v))
;;       (update! (variable-env-others r) n k v)))
;;
;; ;;; 3.2.7 Functions
