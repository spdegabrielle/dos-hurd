#lang racket/base

(require racket/match)

;; TODO: Actually this has a dangerous problem
;;   where it reveals the stamp to the sealed object
(define (make-sealer-pair [name #f])
  ;; cons cells are distinct according to `eq?'
  (define hidden-stamp
    (cons 'hidden 'stamp))
  (define sealed-name
    (if name
        (string->symbol (format "sealed-by-~a" name))
        'sealed))
  (define (sealer val)
    (define (sealed stamp action)
      (match action
        ['unseal
         (if (eq? stamp hidden-stamp)
             val
             (error "Wrong unsealer!"))]
        ['same-stamp?
         (eq? stamp hidden-stamp)]))
    (procedure-rename sealed sealed-name))
  (define (unsealer sealed)
    (sealed hidden-stamp 'unseal))
  (define (sealed? sealed)
    (sealed hidden-stamp 'same-stamp?))
  (values sealer unsealer sealed?))

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
