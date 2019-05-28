#lang racket

(provide cap? rw-cap? read-cap? write-cap?
         cap-can-read? cap-can-write?)

;; TODO: Add nice display representation
(struct cap (name id))
(struct rw-cap cap (sealer unsealer))
(struct read-cap cap (unsealer))
(struct write-cap cap (sealer))

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
  (->* () ((or/c symbol? #f)) rw-cap?)
  (define struct-name
    (if sealer-name
        (string->symbol (string-append "sealed-by-" (symbol->string sealer-name)))
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
