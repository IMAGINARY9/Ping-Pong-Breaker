#lang racket
(require racket/gui)

;;  ---------- DEFINITIONS ----------

; Screen size constants to create gameboard
(define WIDTH 512)
(define HEIGHT 512)

(define PIXEL 16)

;(define FRAME-RATE 17)
(define FRAME-RATE 1)

; (define solid-color  (new brush% [color "colorname"]))
; (define default (new brush% [color "white"][style 'opaque]))

;;  ---------- DEFINITIONS END ----------

;;  ---------- TOOLS ----------

(struct posn (x y) #:transparent)

(define/match (posn-add posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (+ x1 x2) (+ y1 y2))]
)
(define (posn-sum posn1 . other)
  (foldl posn-add posn1 other)
)
(define (posn-sum-lst posn-lst)
  (foldl posn-add (posn 0 0) posn-lst)
)
(define (posn-add-val posn1 val)
  (posn (+ (posn-x posn1) val) 
        (+ (posn-y posn1) val)
))

(define/match (posn-substract posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (- x1 x2) (- y1 y2))]
)

(define/match (posn-multiply posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (* x1 x2) (* y1 y2))]
)
(define (posn-product posn1 . other)
  (foldl posn-multiply posn1 other)
)
(define (posn-multiply-val posn1 val)
  (posn (* (posn-x posn1) val) 
        (* (posn-y posn1) val)
))

(define (posn-normalize p)
  (let ([l (sqrt 
      (+ (* (posn-x p) (posn-x p))
         (* (posn-y p) (posn-y p))))])
            
    (posn (/(posn-x p) l)(/ (posn-y p) l))
  )
)

(struct obj (id position size) #:transparent)

;(struct obj-lst (id lst) #:transparent)

(define (check-hover p obj)
  (and 
    (>= (posn-x p) (posn-x (obj-position obj))) 
      (<= (posn-x p) (+ (posn-x (obj-position obj))
         (posn-x (obj-size obj))))
    (>= (posn-y p)(posn-y (obj-position obj))) 
      (<= (posn-y p) (+ (posn-y (obj-position obj))
         (posn-y (obj-size obj))))
  )
)

;;  ---------- TOOLS END ----------

;;  ---------- BUTTONS ----------

(struct button obj (text))

(define (draw-button dc b)
  (let* ([pos (obj-position b)]
            [sz (obj-size b)])
                
      (send dc draw-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))

  (define-values (text-w text-h d a)
    (send dc get-text-extent (button-text b)))
  (send dc draw-text 
    (button-text b) 
    (+ (posn-x pos) (/ (- (posn-x sz) text-w) 2))
    (+ (posn-y pos) (/ (- (posn-y sz) text-h) 2))
    ;'grapheme
  )
)
)

(define (handle-button button-id)
  (cond
  [(eq? button-id "play-btn") 
    (set-state game-state)
    (send canvas play-game)
  ]
  [(eq? button-id "exit-game-btn") 
    (set-state menu-state)
    (send canvas quit-game)
  ]
  [(eq? button-id "exit-app-btn") (send f show #f)]
  [else (display button-id)]
  )
)

(define play-button (button "play-btn" (posn 128 128) (posn 64 32) "Play"))
(define exit-button (button "exit-app-btn" (posn 256 256) (posn 64 32) "Exit"))
(define exit-game-button (button "exit-game-btn" (posn 256 128) (posn 64 32) "Exit"))

;;  ---------- BUTTONS END ----------

;;  ---------- ENTITIES ----------

(struct entity obj (form))

;;  ---------- PLAYER ----------

(define *player (entity "player" 
  (posn (- (/ WIDTH 2) (* PIXEL 2)) (- HEIGHT (* PIXEL 4))) 
  (posn (* PIXEL 4) PIXEL) "rect"))

(define (move-player x )
  (let ([pl (get-element "player")])
  (unless (false? pl)

  (let ([new-pos (posn 
        (+ (posn-x (obj-position pl)) x)
        (posn-y (obj-position pl)))])
        
    (when (and (> (posn-x new-pos) 0) 
            (< (+ (posn-x new-pos) 
                  (posn-x (obj-size pl))) 
              WIDTH))

      (set-element 
        (struct-copy entity pl
          [position #:parent obj new-pos])
      )
    )
  )
  )
))

;;  ---------- PLAYER END ----------

;;  ---------- BALL ----------

(struct ball entity (direction))

(define *ball (ball "ball"
  (posn (/ WIDTH 2) (/ HEIGHT 2)) 
  (posn PIXEL PIXEL)
  "circle" (posn-normalize (posn 1 1))))

(define (change-ball-direction b new-dir)
  (set-element 
        (struct-copy ball b
          [direction (posn-normalize new-dir)])
      )
)


(define (calc-new-direction current-dir norm-dir)
  (posn-substract current-dir
              (posn-multiply-val (posn-product 
                current-dir norm-dir norm-dir) 2)
            )
)
(define (get-ball-point n angl r c)
  (posn
    (+ (posn-x c)(* r (cos (degrees->radians (+ (* n angl) 90)))))
    (+ (posn-y c)(* r (sin (degrees->radians (+ (* n angl) 90)))))
  )
)

(define (get-ball-points n angl radius center)
  (cond
    [(<= n 0) '()]
    [
      (cons (get-ball-point n angl radius center) 
      (get-ball-points (- n 1) angl radius center))
    ]
  )
)

(define (check-point-col p obj-lst)
  (cond
    [(empty? obj-lst) '()]
    [(check-hover p (first obj-lst))
      (cons p (check-point-col p (rest obj-lst)))
    ]
    [else (check-point-col p (rest obj-lst))]
  )
)
(define (check-points-cols p-lst obj-lst)
  (cond
    [(empty? p-lst) (posn 0 0)]
    [else (posn-sum 
      (posn-sum-lst (check-point-col (first p-lst) obj-lst))
      (check-points-cols (rest p-lst) obj-lst))]
  )
)


(define (check-ball-points b obj-lst n dc)
  (unless (false? b)
    (let* ([angl (/ 360 n)]
            [radius (/ (posn-x (obj-size b)) 2)]
            [center (posn-add-val (obj-position b) radius)]
            [points (get-ball-points n angl radius center)])

      (let ([res (check-points-cols points obj-lst)])

      (unless (equal? res (posn 0 0)) 
      
      (change-ball-direction b
        (calc-new-direction (ball-direction b) (posn-normalize 
          (posn-substract res center))
        )
      )
      ))
    )
   )
)


(define (check-ball-direction b dc)

  ; if (#t and is block) block-hp--
  ; teleport if center is hover
  (check-ball-points b (cons (get-element "player") '()) 1 dc) 
  (check-ball-points b (get-element-lst "block") 16 dc)
  (check-ball-points b (get-element-lst "wall") 4 dc)

  ; change dir
  ; return
  (ball-direction (get-element "ball"))
)

  (define (move-ball dc)
    (let ([b (get-element "ball")])  

    (let* ([new-dir (check-ball-direction b dc)]
            [new-pos (posn-add (obj-position b)
                          new-dir)])

    (set-element 
        (struct-copy ball b
          [position #:parent obj new-pos]
          [direction new-dir])
      )
    )
    )
  )

;;  ---------- BALL END ----------

;;  ---------- WALLS ----------

(define *walls (list 
  (entity "wall" (posn 0 0) (posn PIXEL HEIGHT)
    "rect") 
  (entity "wall" (posn (- WIDTH PIXEL) 0) (posn PIXEL HEIGHT) 
    "rect")
  (entity "wall" (posn 0 0) (posn WIDTH PIXEL) 
    "rect")
  (entity "wall" (posn 0 (- HEIGHT PIXEL PIXEL)) (posn WIDTH PIXEL) 
    "rect"))
)

;;  ---------- WALLS END ----------

;;  ---------- BLOCKS ----------

(struct block entity (hp))

(define (rand-pos-x) (* (random 0 (/ WIDTH PIXEL)) PIXEL))
(define (rand-pos-y) (* (random 0 (/ HEIGHT PIXEL)) PIXEL))
(define (rand-vect2) (posn (rand-pos-x) (rand-pos-y)))

(define *blocks (list 
  (block "block" (posn 320 272) 
    (posn PIXEL PIXEL) "rect" 1) 
  (block "block" (posn 304 288) 
    (posn PIXEL PIXEL) "rect" 1) 
  (block "block" (rand-vect2) 
    (posn PIXEL PIXEL) "rect" 1) 
  (block "block" (rand-vect2) 
    (posn PIXEL PIXEL) "rect" 1)
  (block "block" (rand-vect2)
    (posn PIXEL PIXEL) "rect" 1)
  (block "block" (rand-vect2) 
    (posn PIXEL PIXEL) "rect" 1)
  (block "block" (rand-vect2)
    (posn PIXEL PIXEL) "rect" 1)
))

;;  ---------- BLOCKS END ----------

(define (draw-rectangle dc o)
    (let* ([pos (obj-position o)]
            [sz (obj-size o)])
                
      (send dc draw-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
))

(define (draw-circle dc o)
    (let* ([pos (obj-position o)]
            [sz (obj-size o)])
                
      (send dc draw-ellipse
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
    ) 
)

(define (draw-form dc e)
  (cond
    [(eq? (entity-form e) "rect")
        (draw-rectangle dc e)]
    [(eq? (entity-form e) "circle")
        (draw-circle dc e)]
    [else (displayln `(,(obj-id e) ,(entity-form e)))]
))

(define (draw-entity dc e) 
  (cond
      [(list? e) 
        (for ([sub-e (in-list e)]) 
          (draw-form dc sub-e))
      ]
      [else (draw-form dc e)]
))

;;  ---------- ENTITIES END ----------

;;  ---------- TITLES END ----------

; (struct title (x y))

;;  ---------- TITLES END ----------

;;  ---------- STATES ----------

(struct state (elements buttons))
; (struct state (elements buttons titles))

(define menu-state (state '() (list play-button exit-button)))
(define game-state (state (list *player *ball *blocks *walls) (list exit-game-button)))

(struct state-box (active-state))

(define *current-state (state-box menu-state))

(define (set-state state)
 (set! *current-state (state-box state)))


(define (get-elements)
  (state-elements (state-box-active-state *current-state)))

(define (set-elements elements)
  (set-state (state elements (get-buttons))))


(define (get-buttons)
  (state-buttons (state-box-active-state *current-state)))

(define (set-buttons buttons)
  (set-state (state (get-elements) buttons)))


(define (get-element id) (findf 
  (lambda (el) (equal? (obj-id el) id))
     (get-elements)
))

(define (get-element-lst id) (findf 
  (lambda (el) (and (list? el) (not (empty? el))
         (equal? (obj-id (first el)) id)))
     (get-elements)
))

(define (get-lists main)
  (filter (lambda (lst)
    (and (not (null? lst))(list? lst))) 
      main)
)

(define (get-sub-element id pos)
  (local [(define (get-element-deep id pos where)
            (when (not (false? where))
              (findf (lambda (el) 
                  (and (or (equal? (obj-id el) id) (equal? id "any"))
                      (equal? (obj-position el) pos)))
                where)
          ))]
    (get-element-deep id pos
      (findf (lambda (lst)
        (not (false? (get-element-deep id pos lst))))
        (get-lists (get-elements)))
    )
  )
)

(define (remove-element id)
  (remove (get-element id) (get-elements)))

(define (set-element element)
  (set-elements (cons element (remove-element (obj-id element))))
)

;;  ---------- STATES END ----------

;;  ---------- Game Logic ----------

; f is a Frame
(define f (new frame% [label "PPB"]
               [width WIDTH] [height HEIGHT]
               [border 0]))

; Custom canvas is a Class
; Overridden canvas% class that contains the game logic
(define custom-canvas%
  (class canvas%
    (inherit get-dc)

    (define (tick)
      (move-ball (get-dc))
      (paint-callback this 'y)
    )

    (define game-timer
      (new timer%
           [notify-callback tick] [interval #f])
    )

    (define/public (play-game)
      (send game-timer start FRAME-RATE)
    )

    (define/public (quit-game)
      (send game-timer stop)
    )



    ; Overridden on-char function for checking custom keystrokes on the keyboard 
    (define/override (on-char key-event)
        (define key (send key-event get-key-code))
        
        (cond
            ; base case any key-release has do nothing
            [(equal? key 'release) ]
            ; Left arrow or 'a' key moves the player left
            [(or (equal? key 'left)
                (equal? key #\a ))
                    (move-player (- PIXEL))
                    ;(displayln "move-left")
            ]
            ; Right arrow or 'd' key moves the player right
            [(or (equal? key 'right)
                (equal? key #\d ))
                    (move-player PIXEL)
                    ;(displayln "move-right")
            ]
            [(or (equal? key 'down)
                (equal? key #\s ))
                    (move-ball (get-dc))
                    ;(displayln "move-right")
            ]
            [else (println key)]
        )


        ; Display of any changes
        (paint-callback this 'y)
    )

    (define *x 0)
    (define *y 0)
    (define *on-board #f)

    (define/override (on-event evt)
      (define type (send evt get-event-type))
      (set! *x (send evt get-x))
      (set! *y (send evt get-y))
      (cond
        [(eq? 'leave type) (set! *on-board #f)]
        [(eq? 'enter type) (set! *on-board #t)]
      )

      ;(displayln `("m-pos:", *x, *y))

      (for ([btn (in-list (get-buttons))])
          (when (and (eq? 'left-down type) (check-hover (posn *x *y) btn)) 
          (handle-button (obj-id btn)))
      )

      ; Display of any changes
      (paint-callback this 'y)
      )

    (define (paint-callback _self _evt)
      ; (cond
      ;   ; [(empty? *points)
      ;   ;  (send (send this get-dc) clear)]
        ; [*on-board
        ;  (displayln `("x: ", *x))
        ;  (displayln `("y: ", *y))]
      ;   [else (displayln "out")]
      ;   )
      ;(displayln "p-clbk")
      ; a dc object is a drawing context for drawing graphics and text
      (define dc (get-dc))
      ; Clear old changes
      (send dc clear)

      ;; draw

      (for ([e (in-list (get-elements))])
          (draw-entity dc e)
      )

      (for ([btn (in-list (get-buttons))])
          (draw-button dc btn)
      )

      )

    (super-new [paint-callback paint-callback])
    ))

;;  ---------- Run ----------

; Create a custom-canvas object with frame parent
(define canvas (new custom-canvas% [parent f]))

; Show the game
(send f show #t)