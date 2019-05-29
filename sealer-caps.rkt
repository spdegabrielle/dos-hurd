#lang racket/base

(provide #;cap? rw-cap? read-cap? write-cap?
         readable-cap? writeable-cap?
         new-cap
         cap-pred
         cap-seal cap-unseal
         rw->read-cap rw->write-cap)

(require racket/contract
         racket/match)

(define ((cap-printer prefix) cap port mode)
  (write-string
   (if (cap-name cap)
       (format "#<~a ~a>" prefix
               (cap-name cap))
       (format "#<~a>" prefix))
   port))

(struct cap (name pred sealer unsealer)
  #:methods gen:custom-write
  [(define (write-proc cap port mode)
     (define prefix
       (match cap
         [(and (? cap-sealer) (? cap-unsealer))
          "rw-cap"]
         [(? cap-sealer)
          "read-cap"]
         [(? cap-unsealer)
          "write-cap"]))
     (write-string
      (if (cap-name cap)
          (format "#<~a ~a>" prefix
                  (cap-name cap))
          (format "#<~a>" prefix))
      port))])

(define (truthy val)
  (if val #t #f))

(define (readable-cap? cap)
  (truthy (cap-unsealer cap)))
(define (writeable-cap? cap)
  (truthy (cap-sealer cap)))
(define (rw-cap? cap)
  (truthy (and (cap-sealer cap)
               (cap-unsealer cap))))
(define (read-cap? cap)
  (and (cap-unsealer cap)
       (not (cap-sealer cap))))
(define (write-cap? cap)
  (and (cap-sealer cap)
       (not (cap-unsealer cap))))

(define/contract (cap-seal cap data)
  (-> writeable-cap? any/c any/c)
  ((cap-sealer cap) data))

(define/contract (cap-unseal cap sealed)
  (-> readable-cap? any/c any/c)
  ((cap-unsealer cap) sealed))

(define/contract (new-cap [name #f])
  (->* () ((or/c symbol? string? #f))
       rw-cap?)
  (define struct-name
    (if name
        (string->symbol (format "sealed-by ~a"
                                name))
        'sealed))
  (define-values (struct:seal seal pred seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))

  (define this-cap
    (cap name pred seal unseal))
  this-cap)

(module+ test
  (require rackunit)
  (define foo-cap
    (new-cap 'foo))
  (test-true
   "rw-caps pass readable-cap?"
   (readable-cap? foo-cap))
  (test-true
   "rw-caps pass writeable-cap?"
   (writeable-cap? foo-cap))
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
  (cap (cap-name rw-cap)
       (cap-pred rw-cap)
       #f
       (cap-unsealer rw-cap)))

(define/contract (rw->write-cap rw-cap)
  (-> rw-cap? write-cap?)
  (cap (cap-name rw-cap)
       (cap-pred rw-cap)
       (cap-sealer rw-cap)
       #f))

(module+ test
  (define read-foo-cap
    (rw->read-cap foo-cap))
  (define write-foo-cap
    (rw->write-cap foo-cap))
  (test-true
   "read-caps pass readable-cap?"
   (readable-cap? read-foo-cap))
  (test-true
   "write-caps pass writeable-cap?"
   (writeable-cap? write-foo-cap))
  (test-false
   "write-caps fail readable-cap?"
   (readable-cap? write-foo-cap))
  (test-false
   "read-caps fail writeable-cap?"
   (writeable-cap? read-foo-cap))

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

(define/contract (sealed-by? sealed cap)
  (-> any/c cap? any/c)
  ((cap-pred cap) sealed))

(module+ test
  (define bar-cap
    (new-cap 'bar))

  (test-true
   "sealed-by? in the affirmative"
   (sealed-by? sealed-by-foo foo-cap))
  (test-false
   "sealed-by? in the negative"
   (sealed-by? sealed-by-foo bar-cap)))
