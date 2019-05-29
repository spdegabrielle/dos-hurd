#lang racket/base

;;; Note that this code is highly derivative from dos/win.rkt!

(require racket/match
         racket/list
         dos/os2
         "sealer-caps.rkt")

(struct hurd (env ps))

(define current-sudoer
  (make-parameter #f))

(define (call-with-sudoer sudoer thunk)
  (parameterize ([current-sudoer sudoer])
    (thunk)))

(struct sudoable (sealed-by-cap sealed-by-sudo))

(define (sudoable-seal cap val)
  (define sudoer (current-sudoer))
  (sudoable (cap-seal cap val)
            (and sudoer
                 (cap-seal sudoer val))))

(define (hash-append ht k new-l)
  (define old-l
    (hash-ref ht k #f))
  (hash-set ht k
            (if old-l
                (append new-l old-l)
                new-l)))

(define (hash-cons ht k v)
  (hash-update ht k (λ (old-l) (cons v old-l)) empty))

(define empty-env (hasheq))
(define (merge-env ht1 ht2)
  (for/fold ([ht ht1])
            ([(k v) (in-hash ht2)])
    (hash-append ht k v)))

(define (env-read env read-cap)
  (define sealed-vals
    (hash-ref env (cap-pred read-cap) empty))
  (map (lambda (sv)
         (cap-unseal read-cap
                     (sudoable-sealed-by-cap sv)))
       sealed-vals))
(define (env-read1 env read-cap d)
  (define vs (env-read env read-cap))
  (if (empty? vs) d (first vs)))
(define (sudo-env-read env cap-pred sudoer)
  (define sealed-vals
    (hash-ref env cap-pred empty))
  (map (lambda (sv)
         (cap-unseal sudoer
                     (sudoable-sealed-by-sudo sv)))
       sealed-vals))
(define (sudo-env-read1 env cap-pred sudoer d)
  (define vs (sudo-env-read env cap-pred sudoer))
  (if (empty? vs) d (first vs)))
(define (sudo-env-all env sudoer)
  (for/fold ([ht '#hasheq()])
            ([cap-pred (hash-keys env)])
    (hash-set ht cap-pred
              (sudo-env-read env cap-pred sudoer))))

(define (hasheq/l caps-and-vals)
  (let loop ([h (hasheq)]
             [caps-and-vals caps-and-vals])
    (match caps-and-vals
      [(list)
       h]
      [(list-rest write-cap v caps-and-vals)
       (loop (hash-cons h (cap-pred write-cap)
                        (sudoable-seal write-cap v))
             caps-and-vals)])))

(define (hurd-write #:threads [ts null]
                    . caps-and-vals)
  (os2-write #:threads ts (hasheq/l caps-and-vals)))

(define (hurd-exit . caps-and-vals)
  (os2-exit (hasheq/l caps-and-vals)))

(define (hurd-grub . ts)
  (hurd empty-env ts))

(define (hurd-boot w)
  (match-define (hurd env ps) w)
  (define-values
    (new-env new-ps)
    (os2-boot merge-env env ps empty-env))
  (hurd new-env new-ps))

(define (hurd-env-replace w write-cap vs)
  (match-define (hurd env ps) w)
  (hurd (hash-set env (cap-pred write-cap)
                  (map (lambda (v)
                         (sudoable-seal write-cap v))
                       vs))
        ps))
(define (hurd-env-read w read-cap)
  (match-define (hurd env ps) w)
  (env-read env read-cap))
(define (hurd-env-read1 w read-cap d)
  (match-define (hurd env ps) w)
  (env-read1 env read-cap d))
(define (sudo-hurd-env-read w cap-pred sudoer)
  (match-define (hurd env ps) w)
  (sudo-env-read env cap-pred sudoer))
(define (sudo-hurd-env-read1 w cap-pred sudoer d)
  (match-define (hurd env ps) w)
  (sudo-env-read1 env cap-pred sudoer d))
(define (sudo-hurd-env-all w sudoer)
  (match-define (hurd env ps) w)
  (sudo-env-all env sudoer))

(define (hurd-test env p)
  (os2-test env p))

(require racket/contract/base)
(provide
 (contract-out
  [hurd?
   (-> any/c boolean?)]
  [hurd-grub
   (->* () () #:rest any/c
        hurd?)]
  [hurd-write
   (->* ()
        (#:threads any/c)
        #:rest any/c
        hash?)]
  [hurd-exit
   (->* () ()
        #:rest any/c
        void?)]
  [hurd-test
   (-> hash? (-> any/c any/c)
       hash?)]
  [env-read1
   (-> hash? readable-cap? any/c
       any/c)]
  [env-read
   (-> hash? readable-cap?
       (listof any/c))]
  [hurd-boot
   (-> hurd? hurd?)]
  [hurd-env-replace
   (-> hurd? writeable-cap? (listof any/c)
       hurd?)]
  [hurd-env-read
   (-> hurd? readable-cap?
       (listof any/c))]
  [hurd-env-read1
   (-> hurd? readable-cap? any/c
       any/c)]
  [call-with-sudoer
   (-> writeable-cap? procedure? any/c)]
  [sudo-hurd-env-read
   (-> hurd? procedure? readable-cap?
       (listof any/c))]
  [sudo-hurd-env-read1
   (-> hurd? procedure? readable-cap? any/c
       any/c)]
  [sudo-hurd-env-all
   (-> hurd? readable-cap?
       any/c)]))
