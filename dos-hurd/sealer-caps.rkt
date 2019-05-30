#lang racket/base

(provide #;key? rw-key? read-key? write-key?
         readable-key? writeable-key?
         new-key
         key-pred
         key-seal key-unseal
         rw->read-key rw->write-key)

(require racket/contract
         racket/match)

(define ((key-printer prefix) key port mode)
  (write-string
   (if (key-name key)
       (format "#<~a ~a>" prefix
               (key-name key))
       (format "#<~a>" prefix))
   port))

(struct key (name pred sealer unsealer)
  #:methods gen:custom-write
  [(define (write-proc key port mode)
     (define prefix
       (match key
         [(and (? key-sealer) (? key-unsealer))
          "rw-key"]
         [(? key-sealer)
          "read-key"]
         [(? key-unsealer)
          "write-key"]))
     (write-string
      (if (key-name key)
          (format "#<~a ~a>" prefix
                  (key-name key))
          (format "#<~a>" prefix))
      port))])

(define (truthy val)
  (if val #t #f))

(define (readable-key? key)
  (truthy (key-unsealer key)))
(define (writeable-key? key)
  (truthy (key-sealer key)))
(define (rw-key? key)
  (truthy (and (key-sealer key)
               (key-unsealer key))))
(define (read-key? key)
  (and (key-unsealer key)
       (not (key-sealer key))))
(define (write-key? key)
  (and (key-sealer key)
       (not (key-unsealer key))))

(define/contract (key-seal key data)
  (-> writeable-key? any/c any/c)
  ((key-sealer key) data))

(define/contract (key-unseal key sealed)
  (-> readable-key? any/c any/c)
  ((key-unsealer key) sealed))

(define/contract (new-key [name #f])
  (->* () ((or/c symbol? string? #f))
       rw-key?)
  (define struct-name
    (if name
        (string->symbol (format "sealed-by ~a"
                                name))
        'sealed))
  (define-values (struct:seal seal pred seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))

  (define this-key
    (key name pred seal unseal))
  this-key)

(module+ test
  (require rackunit)
  (define foo-key
    (new-key 'foo))
  (test-true
   "rw-keys pass readable-key?"
   (readable-key? foo-key))
  (test-true
   "rw-keys pass writeable-key?"
   (writeable-key? foo-key))
  (define sealed-by-foo
    (key-seal foo-key 'hello))
  (test-eq?
   "Basic sealing/unsealing"
   (key-unseal foo-key sealed-by-foo)
   'hello)
  (test-exn
   "Trying to unseal with the wrong key fails"
   any/c
   (lambda ()
     (key-unseal (new-key) sealed-by-foo))))

(define/contract (rw->read-key rw-key)
  (-> rw-key? read-key?)
  (key (key-name rw-key)
       (key-pred rw-key)
       #f
       (key-unsealer rw-key)))

(define/contract (rw->write-key rw-key)
  (-> rw-key? write-key?)
  (key (key-name rw-key)
       (key-pred rw-key)
       (key-sealer rw-key)
       #f))

(module+ test
  (define read-foo-key
    (rw->read-key foo-key))
  (define write-foo-key
    (rw->write-key foo-key))
  (test-true
   "read-keys pass readable-key?"
   (readable-key? read-foo-key))
  (test-true
   "write-keys pass writeable-key?"
   (writeable-key? write-foo-key))
  (test-false
   "write-keys fail readable-key?"
   (readable-key? write-foo-key))
  (test-false
   "read-keys fail writeable-key?"
   (writeable-key? read-foo-key))

  (test-eq?
   "read keys can unseal"
   (key-unseal read-foo-key sealed-by-foo)
   'hello)
  (test-eq?
   "write keys can seal"
   (key-unseal foo-key
               (key-seal write-foo-key 'goodbye))
   'goodbye)
  (test-exn
   "read keys can't seal"
   any/c
   (lambda ()
     (key-seal read-foo-key 'yikes)))
  (test-exn
   "write keys can't unseal"
   any/c
   (lambda ()
     (key-unseal write-foo-key
                 (key-seal foo-key 'uhoh)))))

(define/contract (sealed-by? sealed key)
  (-> any/c key? any/c)
  ((key-pred key) sealed))

(module+ test
  (define bar-key
    (new-key 'bar))

  (test-true
   "sealed-by? in the affirmative"
   (sealed-by? sealed-by-foo foo-key))
  (test-false
   "sealed-by? in the negative"
   (sealed-by? sealed-by-foo bar-key)))
