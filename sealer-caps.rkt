#lang racket/base

(provide cap? rw-cap? read-cap? write-cap?
         cap-can-read? cap-can-write?
         new-cap
         cap-id
         cap-seal cap-unseal
         rw->read-cap rw->write-cap)

(require racket/contract
         racket/match)

(define ((cap-printer prefix) cap port mode)
  (write-string
   (if (cap-name cap)
       (format "#<~a ~a>" prefix (cap-name cap))
       (format "#<~a>" prefix))
   port))

;; TODO: Add nice display representation
(struct cap (name id))
(struct rw-cap cap (sealer unsealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "rw-cap"))])
(struct read-cap cap (unsealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "read-cap"))])
(struct write-cap cap (sealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "write-cap"))])

(define (cap-can-read? cap)
  (or (rw-cap? cap) (read-cap? cap)))
(define (cap-can-write? cap)
  (or (rw-cap? cap) (write-cap? cap)))

(define/contract (cap-seal cap data)
  (-> cap-can-write? any/c any/c)
  (define sealer
    (match cap
      [(? write-cap?) (write-cap-sealer cap)]
      [(? rw-cap?) (rw-cap-sealer cap)]))
  (sealer data))

(define/contract (cap-unseal cap sealed)
  (-> cap-can-read? any/c any/c)
  (define unsealer
    (match cap
      [(? read-cap?) (read-cap-unsealer cap)]
      [(? rw-cap?) (rw-cap-unsealer cap)]))
  (unsealer sealed))

(define (new-cap [sealer-name #f])
  (->* () ((or/c symbol? string? #f)) rw-cap?)
  (define struct-name
    (if sealer-name
        (string->symbol (format "sealed-by ~a"
                                sealer-name))
        'sealed))
  (define-values (struct:seal seal sealed? seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))
  (rw-cap sealer-name sealed? seal unseal))

(module+ test
  (require rackunit)
  (define foo-cap
    (new-cap 'foo))
  (test-true
   "rw-caps pass cap-can-read?"
   (cap-can-read? foo-cap))
  (test-true
   "rw-caps pass cap-can-write?"
   (cap-can-write? foo-cap))
  (define sealed-by-foo
    (cap-seal foo-cap 'hello))
  (test-eq?
   "Basic sealing/unsealing"
   (cap-unseal foo-cap sealed-by-foo)
   'hello)
  (test-exn
   "Trying to unseal with the wrong cap fails"
   any/c
   (lambda ()
     (cap-unseal (new-cap) sealed-by-foo))))

(define/contract (rw->read-cap rw-cap)
  (-> rw-cap? read-cap?)
  (read-cap (cap-name rw-cap)
            (cap-id rw-cap)
            (rw-cap-unsealer rw-cap)))

(define/contract (rw->write-cap rw-cap)
  (-> rw-cap? write-cap?)
  (write-cap (cap-name rw-cap)
             (cap-id rw-cap)
             (rw-cap-sealer rw-cap)))

(module+ test
  (define read-foo-cap
    (rw->read-cap foo-cap))
  (define write-foo-cap
    (rw->write-cap foo-cap))
  (test-true
   "read-caps pass cap-can-read?"
   (cap-can-read? read-foo-cap))
  (test-true
   "write-caps pass cap-can-write?"
   (cap-can-write? write-foo-cap))
  (test-false
   "write-caps fail cap-can-read?"
   (cap-can-read? write-foo-cap))
  (test-false
   "read-caps fail cap-can-write?"
   (cap-can-write? read-foo-cap))

  (test-eq?
   "read caps can unseal"
   (cap-unseal read-foo-cap sealed-by-foo)
   'hello)
  (test-eq?
   "write caps can seal"
   (cap-unseal foo-cap
               (cap-seal write-foo-cap 'goodbye))
   'goodbye)
  (test-exn
   "read caps can't seal"
   any/c
   (lambda ()
     (cap-seal read-foo-cap 'yikes)))
  (test-exn
   "write caps can't unseal"
   any/c
   (lambda ()
     (cap-unseal write-foo-cap
                 (cap-seal foo-cap 'uhoh)))))
