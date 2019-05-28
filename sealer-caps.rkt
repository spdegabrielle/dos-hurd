#lang racket/base

(provide #;cap? rw-cap? read-cap? write-cap?
         readable-cap? writeable-cap?
         new-cap
         cap-trademark
         cap-seal cap-unseal
         rw->read-cap rw->write-cap)

(require racket/contract
         racket/match)

(struct trademark (name pred [sudo-sealed #:mutable])
  #:methods gen:custom-write
  [(define (write-proc trademark port mode)
     (write-string
      (if (trademark-name trademark)
          (format "#<trademark ~a>"
                  (trademark-name trademark))
          "#<trademark>")
      port))])

(define ((cap-printer prefix) cap port mode)
  (define trademark (cap-trademark cap))
  (write-string
   (if (trademark-name trademark)
       (format "#<~a ~a>" prefix
               (trademark-name trademark))
       (format "#<~a>" prefix))
   port))

(struct cap (trademark))
(struct rw-cap cap (sealer unsealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "rw-cap"))])
(struct read-cap cap (unsealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "read-cap"))])
(struct write-cap cap (sealer)
  #:methods gen:custom-write
  [(define write-proc (cap-printer "write-cap"))])

(define (readable-cap? cap)
  (or (rw-cap? cap) (read-cap? cap)))
(define (writeable-cap? cap)
  (or (rw-cap? cap) (write-cap? cap)))

(define/contract (cap-seal cap data)
  (-> writeable-cap? any/c any/c)
  (define sealer
    (match cap
      [(? write-cap?) (write-cap-sealer cap)]
      [(? rw-cap?) (rw-cap-sealer cap)]))
  (sealer data))

(define/contract (cap-unseal cap sealed)
  (-> readable-cap? any/c any/c)
  (define unsealer
    (match cap
      [(? read-cap?) (read-cap-unsealer cap)]
      [(? rw-cap?) (rw-cap-unsealer cap)]))
  (unsealer sealed))

(define (new-cap [name #f]
                 ;; rights amplification
                 #:sudo-sealer [sudo-sealer #f])
  (->* () ((or/c symbol? string? #f)
           #:sudo-sealer (or/c writeable-cap? #f))
       rw-cap?)
  (define struct-name
    (if name
        (string->symbol (format "sealed-by ~a"
                                name))
        'sealed))
  (define-values (struct:seal seal sealed? seal-ref seal-set!)
    (make-struct-type struct-name #f 1 0))
  (define unseal
    (make-struct-field-accessor seal-ref 0))

  (define this-trademark
    (trademark name sealed? #f))
  (define this-cap
    (rw-cap this-trademark seal unseal))
  ;; Add rights amplification if sudo-sealer supplied
  (when sudo-sealer
    (set-trademark-sudo-sealed!
     this-trademark
     (cap-seal sudo-sealer
               (make-weak-box this-cap))))
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
  (read-cap (cap-trademark rw-cap)
            (rw-cap-unsealer rw-cap)))

(define/contract (rw->write-cap rw-cap)
  (-> rw-cap? write-cap?)
  (write-cap (cap-trademark rw-cap)
             (rw-cap-sealer rw-cap)))

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
