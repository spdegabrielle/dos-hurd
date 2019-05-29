#lang racket

(require lux
         (prefix-in raart: raart))

(require "../hurd.rkt"
         "../sealer-caps.rkt")

(module+ scratch
  (require rackunit)

  (define ((add-to-cap cap) env)
            (let lp ([ctr 0])
              (hurd-write cap ctr)
              (lp (add1 ctr))))
  (define counter-cap
    (new-cap 'counter))
  (check-equal?
   (hurd-env-read
    (hurd-boot
     (hurd-boot (hurd-grub (add-to-cap counter-cap))))
    counter-cap)
   '(1))

  ;; So one process exiting doesn't affect the others:
  (define ((forkbomb-until cap n) env)
    (let lp ([ctr 0])
      (hurd-write
       #:threads (forkbomb-until cap n)
       cap ctr)
      (if (eqv? n ctr)
          (hurd-exit)
          (lp (add1 ctr)))))
  (define a-cap
    (new-cap 'a))
  (hurd-env-read
   (hurd-boot
    (hurd-boot
     (hurd-boot
      (hurd-boot
       (hurd-boot
        (hurd-grub (forkbomb-until a-cap 1)))))))
   a-cap)
  
  (define sudoer
    (new-cap 'sudo))
  (sudo-hurd-env-all
   (call-with-sudoer
    sudoer
    (lambda ()
      (hurd-boot
       (hurd-boot
        (hurd-boot
         (hurd-boot
          (hurd-boot
           (hurd-grub (forkbomb-until a-cap 1)))))))))
   sudoer)

  ;; Still prints out '(0 1 0 0 1 0 1 0)

  )

(define ((steamer x-pos report-alive) env)
  (define to-height
    (random 1 10))
  (define left? #t)
  
  'TODO
  )

(define (raart-render-game gw [width 80] [height 24])
  (raart:matte
   width height
   (raart:place-cursor-after
    (val->raart
     (hurd-env-read1 (game-hurd gw)
                     (game-display-cap gw)
                     'uhoh)
     (game-hurd gw))
    0 0)))

(struct game
  (;; hurd game state
   hurd
   ;; display capability
   display-cap)
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
     "Cauldron time")
   (define (word-fps gw)
     ;; 30?  60???
     ;; probably 30 is something terminals can reliably
     ;; keep up with...
     30.0)
   (define (word-return gw)
     (void))])

(define cauldron-drawing
  "\
.--------------.
 '--   ----  -'
 /            \\
;              ;
:              :
'.            .'
  '----------'")

(define cauldron-raart
  (raart:vappend*
   #:halign 'left
   (map (lambda (l)
          (raart:text l))
        (string-split cauldron-drawing "\n"))))

(define cauldron-width
  (raart:raart-w cauldron-raart))

(define bubble-max-height 10)

(define ((bubble bubble-display) env)
  (define bubble-lifetime
    (random 2 bubble-max-height))
  (define (modify-drift drift)
    (define drift-it
      (random -1 2))
    (max -1 (min 1 (+ drift drift-it))))
  (define raise-delay (random 5 15))
  (let lp ([x (random 2 (- cauldron-width 2))]
           [y 0]
           [time-till-raise raise-delay]
           [drift 0])
    (when (eqv? y bubble-lifetime)
      (hurd-exit))
    (define time-to-live
      (- bubble-lifetime y))
    (define bubble-shape
      (cond
        ;; big bubbles
        [(>= time-to-live 6)
         #\O]
        ;; medium bubbles
        [(>= time-to-live 2)
         #\o]
        [else #\.]))
    (hurd-write bubble-display
                (list x y bubble-shape))
    (define raise-time?
      (eqv? time-till-raise 0))
    (if raise-time?
        ;; Time to move and adjust
        (lp (max 0        ; drift, but stay within confines
                 (min (+ x drift)
                      (sub1 cauldron-width))) 
            (add1 y)      ; move up
            raise-delay   ; reset
            (modify-drift drift))
        ;; stay the same..
        (lp x y (sub1 time-till-raise) drift))))

(define ((cauldron display) env)
  (define bubble-display-key
    (new-cap 'bubble-display))
  (define bubble-canvas
    (raart:blank cauldron-width bubble-max-height))
  (define (new-bubble-cooldown)
    (random 5 30))
  (let lp ([bubble-cooldown (new-bubble-cooldown)])
    (define bubble-time? (eqv? bubble-cooldown 0))
    (define new-threads
      (if bubble-time?
          (bubble bubble-display-key)
          null))
    (define (do-display env)
      (define all-bubbles
        (hurd-env-read env bubble-display-key))
      (define bubbled-canvas
        (for/fold ([canvas bubble-canvas])
                  ([bubble-info all-bubbles])
          (match bubble-info
            [(list col row char)
             (raart:place-at canvas
                             (sub1 (- bubble-max-height row))
                             col (raart:char char))])))
      (raart:vappend
       #:halign 'center
       ;; green
       (raart:fg 'green
                 bubbled-canvas)
       ;; yellow
       (raart:fg 'yellow
                 cauldron-raart)))
    (hurd-write display do-display
                #:threads new-threads)
    (lp (if bubble-time?
            (new-bubble-cooldown)
            (sub1 bubble-cooldown)))))

(define (val->raart val env)
  (match val
    [(? raart:raart?) val]
    [(? procedure?) (val env)]))

(define (list->raart lst env)
  (map (lambda (v)
         (val->raart v env))
       lst))

(define (start-game)
  (define display-cap
    (new-cap 'display))
  (call-with-chaos
   (raart:make-raart)
   (lambda ()
     (fiat-lux (game (hurd-grub (cauldron display-cap))
                     display-cap))))
  )

(module+ main
  (start-game))
