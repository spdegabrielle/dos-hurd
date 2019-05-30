#lang racket

(require racket/runtime-path
         lux
         (prefix-in raart: raart)
         "../hurd.rkt"
         "../sealer-caps.rkt"
         "./colors.rkt")

(define-runtime-path pwd
  ".")

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


(define (chargrid->raart chargrid [colorgrid #f])
  (define canvas-w
    (apply max
           (for/list ([row chargrid])
             (vector-length row))))
  (define canvas-h
    (vector-length chargrid))
  (define blank-canvas
    (raart:blank canvas-w canvas-h))
  (for/fold ([canvas blank-canvas])
            ([row chargrid]
             [row-i (in-naturals)])
    (for/fold ([canvas canvas])
              ([char row]
               [col-i (in-naturals)])
      (define raart-char
        (raart:char char))
      (define colored-char
        (if colorgrid
            (let* ([color-char (vector-ref (vector-ref colorgrid
                                                       row-i)
                                           col-i)]
                   [color (char-colors-ref color-char)])
              (if color
                  (raart:fg color raart-char)
                  raart-char))
            raart-char))
      (raart:place-at canvas row-i col-i colored-char))))

(define teacup-china-chargrid
  (string->chargrid
   (call-with-input-file (build-path pwd "teacup-china.txt")
     port->string)))
(define teacup-china-colorgrid
  (string->chargrid
   (call-with-input-file (build-path pwd "teacup-china-colors.txt")
     port->string)))
(define teacup-china-raart
  (chargrid->raart teacup-china-chargrid
                   teacup-china-colorgrid))

(define teacup-chockpot-chargrid
  (string->chargrid
   (call-with-input-file (build-path pwd "teacup-chockpot.txt")
     port->string)))
(define teacup-chockpot-colorgrid
  (string->chargrid
   (call-with-input-file (build-path pwd "teacup-chockpot-colors.txt")
     port->string)))
(define teacup-chockpot-raart
  (chargrid->raart teacup-chockpot-chargrid
                   teacup-chockpot-colorgrid))

(define china-steams-chargrid
  (string->chargrid
   (call-with-input-file (build-path pwd "china-steams.txt")
     port->string)))
(define china-steams-raart
  (chargrid->raart china-steams-chargrid))
(define chock-steams-chargrid
  (string->chargrid
   (call-with-input-file (build-path pwd "chock-steams.txt")
     port->string)))
(define chock-steams-raart
  (chargrid->raart chock-steams-chargrid))

(define ((chockpot-teacup display-key) env)
  (define (render env)
    (raart:place-at teacup-chockpot-raart
                    0 0
                    chock-steams-raart))
  (let lp ()
    (hurd-write display-key render)
    (lp)))

(define ((china-teacup display-key) env)
  (define (render env)
    (raart:place-at teacup-china-raart
                    0 0
                    china-steams-raart))
  (let lp ()
    (hurd-write display-key render)
    (lp)))

(define (ljbwt display-key)
  (define teacup-china-key
    (new-key 'teacup-china-key))
  (define teacup-chock-key
    (new-key 'teacup-chock-key))
  (define to-spawn
    (list (china-teacup teacup-china-key)
          (chockpot-teacup teacup-chock-key)))
  (define (process env)
    (define (render env)
      (define china-raart
        (val->raart
         (hurd-env-read1 env
                         teacup-china-key
                         (raart:blank 0 0))
         env))
      (define chock-raart
        (val->raart
         (hurd-env-read1 env
                         teacup-chock-key
                         (raart:blank 0 0))
         env))
      (raart:happend #:valign 'bottom
                     china-raart
                     (raart:blank 5 1)
                     chock-raart))
    (let lp ()
      (hurd-write display-key render)
      (lp)))
  (cons process to-spawn))

(define (start-game)
  (define display-key
    (new-key 'display))
  (call-with-chaos
   (raart:make-raart)
   (lambda ()
     
     (fiat-lux (game (hurd-boot (hurd-grub
                                 (ljbwt display-key)))
                     display-key))))
  (void))

(module+ main
  (start-game))
