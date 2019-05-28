#lang racket/base

(define (make-sealer-pair)
  ;; cons cells are distinct according to `eq?'
  (define hidden-stamp
    (cons 'hidden 'stamp))
  (define (sealer val)
    (define (sealed stamp)
      (if (eq? stamp hidden-stamp)
          val
          (error "Wrong unsealer!")))
    sealed)
  (define (unsealer sealed)
    (sealed hidden-stamp))
  (values sealer unsealer))

(module+ test
  (require rackunit
           racket)

  (define-values (sealer1 unsealer1)
    (make-sealer-pair))
  (define-values (sealer2 unsealer2)
    (make-sealer-pair))

  (test-eq?
   "sealing/unsealing works"
   (unsealer1 (sealer1 'hello))
   'hello)

  (test-exn
   "unsealing with wrong sealer breaks"
   #rx"^Wrong unsealer!"
   (lambda ()
     (unsealer2 (sealer1 'hello))))
  )
