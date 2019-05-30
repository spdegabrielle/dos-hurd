#lang racket

(require lux
         (prefix-in raart: raart))

(require "../hurd.rkt"
         "../sealer-caps.rkt")

(define (val->raart val env)
  (match val
    [(? raart:raart?) val]
    [(? procedure?) (val env)]))

(define (list->raart lst env)
  (map (lambda (v)
         (val->raart v env))
       lst))

(define (raart-render-game gw [width 80] [height 24])
  (raart:matte
   width height
   (raart:place-cursor-after
    (val->raart
     (hurd-env-read1 (game-hurd gw)
                     (game-display-key gw)
                     (raart:blank 0 0))
     (game-hurd gw))
    0 0)))

(struct game
  (;; hurd game state
   hurd
   ;; display keyability
   display-key)
  #:methods gen:word
  [(define (word-tick gw)
     (struct-copy game gw
                  [hurd (hurd-boot (game-hurd gw))]))
   (define (word-event gw e)
     (match e
       ;; quit
       ["q" #f]
       [_ gw]))
   (define (word-output gw)
     (raart-render-game gw))
   (define (word-label gw frame-time)
     "wispy time")
   (define (word-fps gw)
     ;; 30?  60???
     ;; probably 30 is something terminals can reliably
     ;; keep up with...
     30.0)
   (define (word-return gw)
     (void))])

(define (string->chargrid str)
  (for/vector ([row (string-split str "\n")])
    (for/vector ([char row])
      char)))

(define wisp-string
  "\
  .'
 '.
   )
 .'")

(define wisp-chargrid
  (string->chargrid wisp-string))

(define wisp-order
  '((1 3)
    (2 3)
    (3 2)
    (2 1)
    (1 1)
    (2 0)
    (3 0)))

(define ((droplet char lifetime write-to x y) env)
  (for ([i (in-range lifetime)])
    (hurd-write write-to
                (list char x y)))
  (hurd-exit))

(define ((wisp wisp-chargrid wisp-order render-to
               [till-droplet 5]
               [droplet-lifetime 15]
               #:between-wisp-min [between-wisp-min 15]
               #:between-wisp-max [between-wisp-max 45])
         env)
  (define canvas-w
    (apply max
           (for/list ([row wisp-chargrid])
             (vector-length row))))
  (define canvas-h
    (vector-length wisp-chargrid))
  (define blank-canvas
    (raart:blank canvas-w canvas-h))
  (define collect-droplets
    (new-key 'collect-droplets))
  (define (render env)
    (define all-droplets
      (hurd-env-read env collect-droplets))
    (for/fold ([canvas blank-canvas])
              ([droplet-desc all-droplets])
      (match droplet-desc
        [(list char col row)
         (raart:place-at canvas
                         row
                         col (raart:char char))])))
  (let lp ()
    ;; delay for a bit
    (for ([i (in-range (random between-wisp-min
                               between-wisp-max))])
      (hurd-write render-to render))
    ;; now write out all the wisp bits
    (for ([coords wisp-order])
      (match coords
        [(list x y)
         (define char
           (vector-ref (vector-ref wisp-chargrid y) x))
         ;; spawn a new droplet
         (hurd-write render-to render
                     #:threads
                     (droplet char droplet-lifetime
                              collect-droplets
                              x y))
         ;; delay till next droplet
         (for ([i till-droplet])
           (hurd-write render-to render))]))
    (lp)))

(define (start-game)
  (define display-key
    (new-key 'display))
  (call-with-chaos
   (raart:make-raart)
   (lambda ()
     (fiat-lux (game (hurd-boot (hurd-grub (wisp wisp-chargrid
                                                 wisp-order
                                                 display-key)))
                     display-key))))
  (void))

(module+ main
  (start-game))
