#lang racket

(require racket/runtime-path
         lux
         (prefix-in raart: raart)
         ansi
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

(define (raart-render-game gw)
  (define width
    (game-display-cols gw))
  (define height
    (game-display-rows gw))
  (raart:place-cursor-after
   (raart:matte
    width height
    (val->raart
     (hurd-env-read1 (game-hurd gw)
                     (game-display-key gw)
                     (raart:blank 0 0))
     (game-hurd gw)))
   0 0))

(struct game
  (;; hurd game state
   hurd
   ;; display keyability
   display-key
   ;; resolution info
   display-rows
   display-cols)
  #:methods gen:word
  [(define (word-tick gw)
     (struct-copy game gw
                  [hurd (hurd-boot (game-hurd gw))]))
   (define (word-event gw e)
     (match e
       ;; quit
       ["q" #f]
       [(screen-size-report rows cols)
        (struct-copy game gw
                     [display-rows rows]
                     [display-cols cols])]
       [_ gw]))
   (define (word-output gw)
     (raart-render-game gw))
   (define (word-label gw frame-time)
     "Let's Just Be Weird Together")
   (define (word-fps gw)
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
               #:between-wisp-max [between-wisp-max 45]
               #:sub1? [sub1? #t])
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
  (define (render hurd)
    (define all-droplets
      (hurd-env-read hurd collect-droplets))
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
                              (if sub1?
                                  (sub1 x)
                                  x)
                              (if sub1?
                                  (sub1 y)
                                  y)))
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

(define china-steam1-order
  '((12 11)
    (12 10)
    (13 10)
    (14  9)
    (13  8)
    (12  8)
    (11  8)
    (10  9)
    (11  9)
    (12  9)))

(define china-steam2-order
  '((17 11)
    (18 10)
    (18  9)
    (19  9)
    (20  8)
    (19  7)
    (18  7)
    (17  7)
    (16  8)
    (17  8)
    (18  8)))

(define china-steam3-order
  '((24 11)
    (24 10)
    (25 10)
    (26  9)
    (25  8)
    (24  8)
    (23  8)
    (22  9)
    (23  9)
    (24  9)))

(define chock-steam1-order
  '((6 5)
    (7 4)
    (6 3)
    (7 2)))

(define chock-steam2-order
  '((13 4)
    (14 3)
    (15 3)
    (14 2)
    (13 2)
    (14 1)
    (13 1)))

(define chock-steam3-order
  '((20 4)
    (19 4)
    (20 3)
    (19 2)
    (20 1)))

(define ((steam wisp-chargrid wisp-order render-to
                [till-droplet 5]
                [droplet-lifetime 15]
                #:between-wisp-min [between-wisp-min 15]
                #:between-wisp-max [between-wisp-max 45]
                #:sub1? [sub1? #t])
         env)
  (define collect-droplets
    (new-key 'collect-droplets))
  (define (render hurd blit-onto)
    (define all-droplets
      (hurd-env-read hurd collect-droplets))
    (for/fold ([canvas blit-onto])
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
         (when sub1?
           #;(set! x (sub1 x))
           (set! y (sub1 y)))
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

(define (teacup display-key cup-raart
                steam-descs)
  (define steam-onto-key
    (new-key 'steam-onto))
  (define steams
    (for/list ([steam-desc steam-descs])
      (match steam-desc
        [(list steam-chargrid steam-order)
         (steam steam-chargrid steam-order
                steam-onto-key)])))
  (define (process env)
    (define (render hurd)
      (for/fold ([steamed-raart cup-raart])
                ([steamy-renderer
                  (hurd-env-read hurd steam-onto-key)])
        (steamy-renderer hurd steamed-raart)))
    (let lp ()
      (hurd-write display-key render)
      (lp)))
  (cons process steams))

(define (chockpot-teacup display-key)
  (teacup display-key teacup-chockpot-raart
          `((,chock-steams-chargrid ,chock-steam1-order)
            (,chock-steams-chargrid ,chock-steam2-order)
            (,chock-steams-chargrid ,chock-steam3-order))))

(define (china-teacup display-key)
  (teacup display-key teacup-china-raart
          `((,china-steams-chargrid ,china-steam1-order)
            (,china-steams-chargrid ,china-steam2-order)
            (,china-steams-chargrid ,china-steam3-order))))

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
      (define cups-and-steam
        (raart:happend #:valign 'bottom
                       china-raart
                       (raart:blank 5 1)
                       chock-raart))
      (define ljb
        (raart:place-at cups-and-steam
                        0 30 (raart:text "Let's Just Be")))
      (define ljbwt
        (raart:place-at ljb
                        23 30 (raart:text "Weird Together")))
      ljbwt)
    (let lp ()
      (hurd-write display-key render)
      (lp)))
  (cons process to-spawn))

(define (display* . things)
  (for-each display things)
  (flush-output))

(define (with-clean-tty proc)
  (tty-raw!)
  (display* (dec-soft-terminal-reset)
            (device-request-screen-size))
  (match-define (screen-size-report rows columns)
    (lex-lcd-input (current-input-port)))
  (define proc-result
    (proc rows columns))
  (display* (dec-soft-terminal-reset)
            (clear-screen/home))
  proc-result)

(define (start-game)
  (with-clean-tty
    (lambda (rows cols)
      (define display-key
        (new-key 'display))
      (call-with-chaos
       (raart:make-raart)
       (lambda ()
         (fiat-lux (game (hurd-boot (hurd-grub
                                     (ljbwt display-key)))
                         display-key
                         rows cols))))))
  (void))

(module+ main
  (start-game))
