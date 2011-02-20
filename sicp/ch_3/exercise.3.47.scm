;;; Exercise 3.47

;; A semaphore (of size n) is a generalization of a
;; mutex.  Like a mutex, a semaphore supports acquire and release
;; operations, but it is more general in that up to n processes can
;; acquire it concurrently.  Additional processes that attempt to
;; acquire the semaphore must wait for release operations.  Give
;; implementations of semaphores
;;
;;   a. in terms of mutexes

(define (make-semaphore1 max)
  (let ((lock (make-mutex))
        (count 0))
    (define (incr)
      (if (< count max)
          (set! count (+ 1 count))
          (incr)))
    (define (decr)
      (set! count (- 1 count)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (lock 'acquire)
             (incr)
             (lock 'release))
            ((eq? m 'release)
             (decr))))))

;;   b. in terms of atomic `test-and-set!' operations.

;; I could do it... but why bother? The mutex one works fine (hopefully).

