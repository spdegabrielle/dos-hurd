#lang racket

(provide char-colors
         char-colors-ref)

(define char-colors
  '#hasheq((#\k . black)
           (#\r . red)
           (#\g . green)
           (#\y . yellow)
           (#\b . blue)
           (#\m . magenta)
           (#\c . cyan)
           (#\w . white)
           (#\K . brblack)
           (#\R . brred)
           (#\G . brgreen)
           (#\Y . bryellow)
           (#\B . brblue)
           (#\M . brmagenta)
           (#\C . brcyan)
           (#\W . brwhite)
           ;; These are defaults, so
           #;(#\- . #f)
           #;(#\space . #f)))

(define inverse-char-colors
  (for/fold ([ht #hasheq()])
            ([(char color) char-colors])
    (if color
        (hash-set ht color char)
        ht)))

(define (char-colors-ref char)
  (hash-ref char-colors char #f))

(module+ main
  (define colors
    '((black    red       green    yellow)
      (blue     magenta   cyan     white)
      (brblack  brred     brgreen  bryellow)
      (brblue   brmagenta brcyan   brwhite)))

  (define (color-text color)
    (define char
      (hash-ref inverse-char-colors color))
    (fg color
        (text (format "~a: ~a"
                      (list->string (list char))
                      (symbol->string color)))))

  (require raart)
  (draw-here
   (vappend
    #:halign 'center

    (text "NORMAL")
    (text "======")

    (table (for/list ([row colors])
             (for/list ([color row])
               (color-text color))))

    (text "")
    (style 'bold (text "BOLD"))
    (style 'bold (text "===="))

    (table (for/list ([row colors])
             (for/list ([color row])
               (style 'bold
                      (color-text color)))))))
  (newline))
