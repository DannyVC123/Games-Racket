;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname snake) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "./snake_lib_edited.rkt")

; a game is...
; - (make-game snake (listof posn) (listof posn) number)
; (define-struct game (snake food obstacles ticks))

; a direction is one of...
; - 'up
; - 'down
; - 'left
; - 'right
; If this type looks new to you, its just a symbol.
; That is ‘up is a symbol and “up” is a string.
; Symbols are like strings without spaces. 


; a snake is...
; - (make-snake direction (listof posn))
; (define-struct snake (heading segments))

; segments is either
; - (cons posn empty)
; - (cons posn segments)
; That is, segments is a non-empty list of posns. 
; x-coordinates increase from 1 to board-length (inclusive) toward the right
; y-coordinates increase from 1 to board-length (inclusive) toward the top
; the default value for board-length is 50.

; food is either
; - empty
; - (cons posn food)
; That is, food is a list of posns.

; obstacles is either
; - empty
; - (cons posn obstacles)
; Obstacles is also a list of posns.

; add-item: (listof posn) posn -> game
; Given a list of food/obstacles/etc. and a posn, returns a list with the new posn added to the front
(define (add-item lst new-posn)
  (cons new-posn lst))

; change-snake-direction: game direction -> game
; Given a game and direction, returns a new game where the snake
;   is now headed in the provided direction. 
(define (change-snake-direction g d)
  (make-game (make-snake d
                         (snake-segments (game-snake g)))
             (game-food g)
             (game-obstacles g)
             (game-ticks g)))

; game-score : game -> number
; Given a game, returns a score (as a number)
(define (game-score g)
  (max (- (* (length (snake-segments (game-snake g)))
             100)
          (game-ticks g))
       0))

; game-over? : game -> boolean
; Given a game, returns true if that snake has died and false otherwise.
; We strongly recommend writing helper functions for this question!
(define (game-over? g)
  (local [(define segments (snake-segments (game-snake g)))
          (define head (first (snake-segments (game-snake g))))
          ; hit-snake: true if the snake has hit itself
          (define hit-snake (and (>= (length segments) 3)
                                 (member head (rest segments))))
          ; hit-wall: true if the snake has hit a wall
          (define hit-wall
            (local [(define x (posn-x head))
                    (define y (posn-y head))]
              (or (< x 1) (> x 25)
                  (< y 1) (> y 25))))
          ; hit-obstacle: true if the snake has hit an obstacle
          (define hit-obstacle
            (member head (game-obstacles g)))]
    (or hit-snake
        hit-wall
        hit-obstacle)))

; game-advance: game -> game
; Takes a game as input and advances the game one tick. The snake
;  moves forward one segment and eats or not. 
(define (game-advance g)
  (local [(define snake (game-snake g))
          (define segments (snake-segments snake))
          (define head (first (snake-segments snake)))
          (define food (game-food g))
          ; -------------------------------------------------------------------------------------------------------------------
          ; posn=?: posn posn -> boolean
          ; returns true if the two positions are equal
          (define (posn=? posn1 posn2)
            (and (= (posn-x posn1) (posn-x posn2))
                 (= (posn-y posn1) (posn-y posn2))))
          ; -------------------------------------------------------------------------------------------------------------------
          (define direction (snake-heading snake))
          (define direc-lst (cond [(symbol=? direction 'up)    '( 0  1)]
                                  [(symbol=? direction 'down)  '( 0 -1)]
                                  [(symbol=? direction 'left)  '(-1  0)]
                                  [(symbol=? direction 'right) '( 1  0)]))
          ; new head
          (define new-head (make-posn (+ (posn-x head) (first  direc-lst))
                                      (+ (posn-y head) (second direc-lst))))
          ; -------------------------------------------------------------------------------------------------------------------
          ; remove-food-if-eaten: (listof posn) posn -> (listof posn)
          ; if the snake ran into a piece of food, it will be removed from the food list
          (define (remove-food-if-eaten food new-head)
            (cond [(empty? food) '()]
                  [else (local [(define food-head (first food))]
                          (cond [(posn=? food-head new-head) (rest food)]
                                [else                        (cons food-head
                                                                   (remove-food-if-eaten (rest food)
                                                                                         new-head))]))]))
          ; new-food
          (define new-food (remove-food-if-eaten food new-head))
          ; -------------------------------------------------------------------------------------------------------------------
          ; new segments
          (define new-segments (cons new-head
                                     (cond [(< (length new-food) (length food)) segments]
                                           [else                                (reverse (rest (reverse segments)))])))]
    (make-game (make-snake direction new-segments)
               new-food
               (game-obstacles g)
               (+ (game-ticks g) 1))))

; a starting game to experiment with
(define game-start
  (make-game (make-snake 'up (list (make-posn 12 12)))
             (list (make-posn 2 2) 
                   (make-posn 5 20)
                   (make-posn 15 15)
                   (make-posn 24 24))
             empty
             0))

;; play : game -> game
(define (play initial-game)
  (play-game initial-game game-advance add-item change-snake-direction game-score game-over?))

; to start a game
(play game-start)