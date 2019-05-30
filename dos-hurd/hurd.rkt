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

(struct sudoable (sealed-by-key sealed-by-sudo))

(define (sudoable-seal key val)
  (define sudoer (current-sudoer))
  (sudoable (key-seal key val)
            (and sudoer
                 (key-seal sudoer val))))

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

(define (env-read env read-key)
  (define sealed-vals
    (hash-ref env (key-pred read-key) empty))
  (map (lambda (sv)
         (key-unseal read-key
                     (sudoable-sealed-by-key sv)))
       sealed-vals))
(define (env-read1 env read-key d)
  (define vs (env-read env read-key))
  (if (empty? vs) d (first vs)))
(define (sudo-env-read env key-pred sudoer)
  (define sealed-vals
    (hash-ref env key-pred empty))
  (map (lambda (sv)
         (key-unseal sudoer
                     (sudoable-sealed-by-sudo sv)))
       sealed-vals))
(define (sudo-env-read1 env key-pred sudoer d)
  (define vs (sudo-env-read env key-pred sudoer))
  (if (empty? vs) d (first vs)))
(define (sudo-env-all env sudoer)
  (for/fold ([ht '#hasheq()])
            ([key-pred (hash-keys env)])
    (hash-set ht key-pred
              (sudo-env-read env key-pred sudoer))))

(define (hasheq/l keys-and-vals)
  (let loop ([h (hasheq)]
             [keys-and-vals keys-and-vals])
    (match keys-and-vals
      [(list)
       h]
      [(list-rest write-key v keys-and-vals)
       (loop (hash-cons h (key-pred write-key)
                        (sudoable-seal write-key v))
             keys-and-vals)])))

(define (hurd-write #:threads [ts null]
                    . keys-and-vals)
  (os2-write #:threads ts (hasheq/l keys-and-vals)))

(define (hurd-exit . keys-and-vals)
  (os2-exit (hasheq/l keys-and-vals)))

(define (hurd-grub . ts)
  (hurd empty-env ts))

(define (hurd-boot w)
  (match-define (hurd env ps) w)
  (define-values
    (new-env new-ps)
    (os2-boot merge-env env ps empty-env))
  (hurd new-env new-ps))

(define (hurd-env-replace w write-key vs)
  (match-define (hurd env ps) w)
  (hurd (hash-set env (key-pred write-key)
                  (map (lambda (v)
                         (sudoable-seal write-key v))
                       vs))
        ps))
(define (hurd-env-read w read-key)
  (match-define (hurd env ps) w)
  (env-read env read-key))
(define (hurd-env-read1 w read-key d)
  (match-define (hurd env ps) w)
  (env-read1 env read-key d))
(define (sudo-hurd-env-read w key-pred sudoer)
  (match-define (hurd env ps) w)
  (sudo-env-read env key-pred sudoer))
(define (sudo-hurd-env-read1 w key-pred sudoer d)
  (match-define (hurd env ps) w)
  (sudo-env-read1 env key-pred sudoer d))
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
  [hurd-env
   (-> hurd? hash?)]
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
   (-> hash? readable-key? any/c
       any/c)]
  [env-read
   (-> hash? readable-key?
       (listof any/c))]
  [hurd-boot
   (-> hurd? hurd?)]
  [hurd-env-replace
   (-> hurd? writeable-key? (listof any/c)
       hurd?)]
  [hurd-env-read
   (-> hurd? readable-key?
       (listof any/c))]
  [hurd-env-read1
   (-> hurd? readable-key? any/c
       any/c)]
  [call-with-sudoer
   (-> writeable-key? procedure? any/c)]
  [sudo-hurd-env-read
   (-> hurd? procedure? readable-key?
       (listof any/c))]
  [sudo-hurd-env-read1
   (-> hurd? procedure? readable-key? any/c
       any/c)]
  [sudo-hurd-env-all
   (-> hurd? readable-key?
       any/c)]))
