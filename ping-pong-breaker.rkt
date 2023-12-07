#lang racket
(require racket/gui)
(require rackunit)

;;  ---------- DEFINITIONS ----------

; Screen size constants to create gameboard
(define PIXEL 16)

(define WIDTH 512)
(define HEIGHT 768)

;(define FRAME-RATE 17) ;60fps
;(define FRAME-RATE 8) ;120fps
(define FRAME-RATE 4) ;250fps
;(define FRAME-RATE 1) ;1000fps

;(define solid-color  (new brush% [color "colorname"]))
(define brush-button  (new brush% [color (make-object color% 250 210 205)]))

(define brush-ball (new brush% [color (make-object color% 60 60 125)]))
(define brush-player (new brush% [color (make-object color% 250 210 205)]))
(define brush-wall (new brush% [color (make-object color% 240 140 135)]))
(define brush-block1 (new brush% [color (make-object color% 250 210 170)]))
(define brush-block2 (new brush% [color (make-object color% 250 160 135)]))
(define brush-block3 (new brush% [color (make-object color% 245 125 130)]))
(define background-color (make-object color% 160 85 120))

(define default (new brush% [color "white"][style 'opaque]))

;;  ---------- DEFINITIONS END ----------

;;  ---------- TOOLS ----------

;; Data type
;; posn is a structure (posn x y)  
;; interpretation: a prefab struct representing a point in 2d space
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

; border is a posn
(define BLOCKS-BORDER-LU (posn 1 (/ (/ HEIGHT PIXEL) 8)))
(define BLOCKS-BORDER-RD (posn (- (/ WIDTH PIXEL) 1) (/ (/ HEIGHT PIXEL) 2)))

(define MAX-BLOCKS-NUMBER 
  (* (- (posn-y BLOCKS-BORDER-RD) (posn-y BLOCKS-BORDER-LU)) 
      (- (posn-x BLOCKS-BORDER-RD) (posn-x BLOCKS-BORDER-LU)))
) ;30x8 ;240

;; Data type
;; obj is a structure (obj id position size)
;; where id is a Number, position and size is a posn
;; interpretation: a prefab struct representing object in application

(struct item (id) #:transparent)
(struct obj item (position size brush) #:transparent)
(struct obj-list item (lst) #:transparent)

; check-hover : posn, obj -> Boolean
; does the point overlap with the object
; header: (define (check-hover p obj)
; template: (define (check-hover p obj) (and ... p obj ...))
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

; tests
(check-equal? (check-hover (posn 150 150) 
  (obj "test-hover-obj" (posn 120 120) (posn 50 50) brush-button)) #t)
(check-equal? (check-hover (posn 150 150) 
  (obj "test-hover-obj" (posn 200 100) (posn 50 50) brush-button)) #f)

;;  ---------- TOOLS END ----------

;;  ---------- BUTTONS ----------

;; Data type
;; button is a structure inherited from an obj (button id position size text)
;; where text is a String
;; interpretation: a prefab struct representing button in application
(struct button obj (text))

; draw-button : dc, b -> Void
; draw button on canvas
; header: (define (draw-button dc b)
; template: (define (draw-button dc b) (send dc ... b ...))
(define (draw-button dc b)
  (let* ([pos (obj-position b)]
            [sz (obj-size b)])

      (send dc set-brush (obj-brush b))
                
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

; handle-button : button-id -> Void
; button handling by button id
; header: (define (handle-button button-id)
; template: (define (handle-button button-id) (cond ... button-id ...)
(define (handle-button button-id)
  (cond
  [(eq? button-id "play-btn") 
    (set-state game-state)
    (send canvas init-game)
  ]
  [(eq? button-id "exit-game-btn") 
    (set-state menu-state)
    (send canvas quit-game)
  ]
  [(eq? button-id "pause-game-btn") 
    (send canvas quit-game)
    (remove-button "pause-game-btn")
    (add-button continue-game-button)
  ]
  [(eq? button-id "continue-game-btn") 
    (send canvas play-game)
    (remove-button "continue-game-btn")
    (add-button pause-game-button)
  ]
  [(eq? button-id "exit-app-btn") (send f show #f)]
  [else (display button-id)]
  )
  (send canvas paint-call)
)

(define play-button (button "play-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 4 2)) (/ HEIGHT 3)) 
  (posn (/ WIDTH 4) (/ HEIGHT 20)) brush-button "Play"))

(define exit-button (button "exit-app-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 4 2)) (/ HEIGHT 2)) 
  (posn (/ WIDTH 4) (/ HEIGHT 20)) brush-button "Exit"))

(define exit-game-button (button "exit-game-btn" 
  (posn 16 16) (posn 64 32) brush-button "Exit"))

(define pause-game-button (button "pause-game-btn" 
  (posn 256 128) (posn 64 32) brush-button "Pause"))

(define continue-game-button (button "continue-game-btn" 
  (posn 256 128) (posn 64 32) brush-button "Continue"))

;;  ---------- BUTTONS END ----------

;;  ---------- ENTITIES ----------

;; Data type
;; entity is a structure inherited from an obj (entity id position size form)
;; where form is a String
;; interpretation: a prefab struct representing game entity that can be drawn by form
(struct entity obj (form))

;;  ---------- PLAYER ----------

;; player is an initial manipulable entity, your actor
(define *player (entity "player" 
  (posn (- (/ WIDTH 2) (* PIXEL 2)) (- HEIGHT (* PIXEL 4))) 
  (posn (* PIXEL 4) PIXEL) brush-player "round-rect"))

; move-player : Number -> Void
; move player on by x coord
; header: (define (move-player x)
; template: (move-player x) (... x ...)
(define (move-player x)
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

;; Data type
;; ball is a structure inherited from an entity (ball id position size form direction)
;; where direction is a posn
;; interpretation: a prefab struct representing game ball
(struct ball entity (direction))

;; *ball is a initial game ball
(define *ball (ball "ball"
  (posn (/ WIDTH 2) (/ HEIGHT 2)) 
  (posn PIXEL PIXEL) brush-ball
  "circle" (posn-normalize (posn 0 1))))

(define (calc-new-direction current-dir norm-dir)
  (posn-substract current-dir
              (posn-multiply-val (posn-product 
                current-dir norm-dir norm-dir) 2)
            )
)
(define (get-ball-point n angl r c offset)
  (posn
    (+ (posn-x c)(* r (cos (degrees->radians (+ (* n angl) offset)))))
    (+ (posn-y c)(* r (sin (degrees->radians (+ (* n angl) offset)))))
  )
)

(define (get-ball-points n angl radius center offset)
  (cond
    [(<= n 0) '()]
    [(cons (get-ball-point n angl radius center offset) 
      (get-ball-points (- n 1) angl radius center offset))
    ]
  )
)

(define (get-point-cols p el lst)
  (cond
    [(empty? lst) '()]
    [(check-hover p (first lst))
      (when (block? (first lst))
        (block-collide el (first lst)))
      (cons p (get-point-cols p el (rest lst)))
    ]
    [else (get-point-cols p el (rest lst))]
  )
)

(define (point-cols p el)
  (if (obj-list? el)
    (get-point-cols p el (obj-list-lst el))
    (if (check-hover p el)
      (cons p '()) '()
    )
  )
)

(define (get-points-cols p-lst el)
  (cond
    [(empty? p-lst) (posn 0 0)]
    [else (posn-sum 
      (posn-sum-lst (point-cols (first p-lst) el))
      (get-points-cols (rest p-lst) el))]
  )
)

(define (get-ball-cols b el points-num [offset 0])
  (unless (false? b)
    (let* ([angl (/ 360 points-num)]
            [radius (/ (posn-x (obj-size b)) 2)]
            [center (posn-add-val (obj-position b) radius)]
            [points (get-ball-points points-num angl radius center offset)]
            [res (get-points-cols points el)])

      (if (equal? res (posn 0 0)) (posn 0 0)
        (calc-new-direction (ball-direction b) 
           (posn-normalize (posn-substract res center)))
      )
    )
   )
)

(define (get-all-colls b)
  (posn-sum
    (get-ball-cols b (get-element "player") 1 90)
    (get-ball-cols b (get-element "blocks") 16)
    (get-ball-cols b (get-element "enemies") 16)
    (get-ball-cols b (get-element "walls") 4)
  )
)
; change-ball-direction : b new-dir -> Void
; set new value to direction of game ball
; header: (define (change-ball-direction b new-dir)
; template: (define (change-ball-direction b new-dir) (... b new-dir ...)
(define (change-ball-direction new-dir)
  ;(displayln new-dir)
  (unless (equal? new-dir (posn 0 0))
    (let ([b (get-element "ball")])
      (set-element (struct-copy ball b
          [direction (posn-normalize
            (cond
              [(integer? (posn-x new-dir)) (posn-add new-dir (posn 0.1 0.5))]
              [(integer? (posn-y new-dir)) (posn-add new-dir (posn -0.5 -0.1))]
              [else new-dir]
            )
          )]
      )
    ))
  )
)

(define (move-ball)
  (let ([b (get-element "ball")])
    (set-element (struct-copy ball b
          [position #:parent obj 
            (posn-add (obj-position b) 
            (ball-direction b))])
    ))
)

(define (update-ball)
    (move-ball)
    (change-ball-direction
     (get-all-colls (get-element "ball")))
)

;;  ---------- BALL END ----------

;;  ---------- WALLS ----------

; *walls is an initial List<Entity>
(define *walls (obj-list "walls" (list 
  (entity "wall0" (posn 0 0) (posn PIXEL HEIGHT)
    brush-wall "rect") 
  (entity "wall1" (posn (- WIDTH PIXEL) 0) (posn PIXEL HEIGHT) 
    brush-wall "rect")
  (entity "wall2" (posn 0 0) (posn WIDTH PIXEL) 
    brush-wall "rect")
  (entity "wall3" (posn 0 (- HEIGHT PIXEL PIXEL)) (posn WIDTH PIXEL) 
    brush-wall "rect")
    )
))

;;  ---------- WALLS END ----------

;;  ---------- BLOCKS ----------

;; Data type
;; block is a structure inherited from an entity (block id position size form hp)
;; where hp is a Number
;; interpretation: a prefab struct representing block
(struct block entity (hp))

; *blocks is an initial List<block>
(define *blocks (obj-list "blocks" (list 
  (block "block0" (posn 320 272) 
    (posn PIXEL PIXEL) brush-block1 "rect" 2) 
  (block "block1" (posn 304 288) 
    (posn PIXEL PIXEL) brush-block1 "rect" 2) 
  )
))

(define (generate-block-posn border-l border-r)
  (posn (* (random (posn-x border-l) (posn-x border-r)) PIXEL)
        (* (random (posn-y border-l) (posn-y border-r)) PIXEL)
))

(define (generate-blocks-posns acc n border-l border-r)
  (if (<= n 0) acc
    (let ([r-posn (generate-block-posn border-l border-r)])
      (if (member r-posn acc)
        (generate-blocks-posns acc n border-l border-r)
        (generate-blocks-posns (cons r-posn acc) (- n 1) border-l border-r)
      )
    )
  )
)

(define (get-block-brush hp)
  (cond
    [(= hp 1) brush-block1]
    [(= hp 2) brush-block2]
    [(= hp 3) brush-block3]
    [else default]
  )
)

(define (generate-blocks-list posn-lst)
  (if (empty? posn-lst) '()
    (cons (let ([hp (random 1 4)]) 
      (block (~a "block" (length posn-lst)) 
        (first posn-lst) (posn PIXEL PIXEL) 
        (get-block-brush hp) "rect" hp))
      (generate-blocks-list (rest posn-lst)))
  )
)

(define (generate-blocks n)
  (obj-list "blocks" (generate-blocks-list 
    (generate-blocks-posns '() 
      (if (< n MAX-BLOCKS-NUMBER) n MAX-BLOCKS-NUMBER)
     BLOCKS-BORDER-LU BLOCKS-BORDER-RD))
  )
)


(define (block-collide lst blk)
  ;(let ([lst (get-element "blocks")])
    (if (<= (block-hp blk) 1)
      (remove-sub-element (item-id blk) lst)
      (let ([new-hp (- (block-hp blk) 1)])
        (set-sub-element 
        (struct-copy block blk
          [hp new-hp]
          [brush #:parent obj (get-block-brush new-hp)]
        ) lst)
      )
    )
)

;;  ---------- BLOCKS END ----------

;;  ---------- ENEMIES ----------

;; Data type
;; enemy is a block
;; where hp is a Number
;; interpretation: a prefab struct representing block

; *enemies is an initial List<Entity>
(define *enemies (obj-list "enemies" (list 
  (block "enemy0" (posn (- (/ WIDTH 2) (/ PIXEL 2)) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy1" (posn (- (* WIDTH 0.25) PIXEL) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy2" (posn (* WIDTH 0.75) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  )
))

;;  ---------- ENEMIES END ----------

; draw-rectangle : dc, o -> Void
; draw o - object rectangle form on canvas
; header: (define (draw-rectangle dc o)
; template: (define (draw-rectangle dc o) (send dc draw-rectangle ... o ...))
(define (draw-rectangle dc o)
    (let* ([pos (obj-position o)]
            [sz (obj-size o)])
                
      (send dc set-brush (obj-brush o))

      (send dc draw-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
))

; draw-rounded-rectangle : dc, o -> Void
; draw o - object rounded rectangle form on canvas
; header: (define (draw-rounded-rectangle dc o)
; template: (define (draw-rounded-rectangle dc o) (send dc draw-rounded-rectangle ... o ...))
(define (draw-rounded-rectangle dc o)
    (let* ([pos (obj-position o)]
            [sz (obj-size o)])
                
      (send dc set-brush (obj-brush o))

      (send dc draw-rounded-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz)
        5
        )
))

; draw-circle : dc, o -> Void
; draw o - object circle form on canvas
; header: (define (draw-circle dc o)
; template: (define (draw-circle dc o) (send dc draw-ellipse ... o ...))
(define (draw-circle dc o)
    (let* ([pos (obj-position o)]
            [sz (obj-size o)])

      (send dc set-brush (obj-brush o))
                
      (send dc draw-ellipse
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
    ) 
)

; draw-form : dc, el -> Void
; call the function depend on element form
; header: (define (draw-form dc el)
; template: (define (draw-form dc el) (cond ... (entity-form el) ...))
(define (draw-form dc el)
  (cond
    [(eq? (entity-form el) "rect")
        (draw-rectangle dc el)]
    [(eq? (entity-form el) "round-rect")
        (draw-rounded-rectangle dc el)]
    [(eq? (entity-form el) "circle")
        (draw-circle dc el)]
    [else (displayln `(,(item-id el) ,(entity-form el)))]
))

; draw-element : dc, el -> Void
; call the function depend on element type
; header: (define (draw-element dc el)
; template: (define (draw-element dc el) (cond ... (el ...) ...)
(define (draw-element dc el) 
  (cond
      [(obj-list? el)
        (for ([sub-el (in-list (obj-list-lst el))]) 
          (draw-form dc sub-el))
      ]
      [else (draw-form dc el)]
))

;;  ---------- ENTITIES END ----------

;;  ---------- TITLES END ----------

; (struct title (x y))

;;  ---------- TITLES END ----------

;;  ---------- STATES ----------

;; Data type
;; element is an any game element
;; - obj
;; - button ...

;; Data type
;; state is a structure (state elements buttons)
;; where elements, buttons is a List<obj>
;; interpretation: a prefab struct representing world-state  
(struct state (elements buttons))
; (struct state (elements buttons titles))

(define menu-state (state '() (list play-button exit-button)))
(define game-state (state (list *player *ball *blocks *walls *enemies) 
                          (list pause-game-button exit-game-button)))

;; Data type
;; state-box is a structure (state active-state)
;; where active-state is a state
;; interpretation: a prefab struct contain active world-state
(struct state-box (active-state))

; *current-state is an initial state 
(define *current-state (state-box menu-state))

; set-state : dc, el -> Void
; set new current state
; header: (define (set-state state)
; template: (define (set-state state) (set! ... state ...)
(define (set-state state)
 (set! *current-state (state-box state)))

; get-elements : -> List<Elements>
; get list of current elements
; header: (define (get-elements)
; template: (define (get-elements) (...)
(define (get-elements)
  (state-elements (state-box-active-state *current-state)))

; test
(check-equal? (get-elements) '())

; set-elements : List<Elements> -> Void
; set new current elements
; header: (define (set-elements elems)
; template: (define (set-elements elems) (...)
(define (set-elements elems)
  (set-state (state elems (get-buttons))))

; get-buttons : -> List<Buttons>
; get list of current buttons
; header: (define (get-buttons)
; template: (define (get-buttons) (...)
(define (get-buttons)
  (state-buttons (state-box-active-state *current-state)))

; test
(check-equal? (get-buttons) (list play-button exit-button))

; set-buttons : List<Buttons> -> Void
; set new current buttons
; header: (define (set-buttons buttons)
; template: (define (set-buttons buttons) (...)
(define (set-buttons buttons)
  (set-state (state (get-elements) buttons)))

; get-element : String -> obj
; get element by id
; header: (define (get-element id)
; template: (define (get-element id) (findf (lambda (...) ... id ...) ...)
(define (get-button id) (findf 
  (lambda (el) (equal? (item-id el) id))
     (get-buttons)
))

(define (add-button el)
  (set-buttons (cons el (get-buttons)))
)

; remove-element : String -> Void
; remove element by id
; header: (define (remove-element id)
; template: (define (remove-element id) (remove (...) ...)
(define (remove-button id)
  (set-buttons (remove (get-button id) (get-buttons)))
)


; get-element : String -> obj
; get element by id
; header: (define (get-element id)
; template: (define (get-element id) (findf (lambda (...) ... id ...) ...)
(define (get-element id) (findf 
  (lambda (el) (equal? (item-id el) id))
     (get-elements)
))

; test
(check-equal? (get-element "player") #f)

(define (add-element el)
  (set-elements (cons el (get-elements)))
)

; remove-element : String -> Void
; remove element by id
; header: (define (remove-element id)
; template: (define (remove-element id) (remove (...) ...)
(define (remove-element id)
  (set-elements (remove (get-element id) (get-elements)))
)

; set-element : obj -> Void
; remove element by id
; header: (define (set-element id)
; template: (define (set-element id) (remove (...) ...)
(define (set-element el)
  (remove-element (item-id el))
  (add-element el)
)

; get-sub-element : String, List<obj> -> obj
; get element from lists by id and position
; header: (define (get-sub-element id el-lst)
(define (get-sub-element id el-lst)(findf 
  (lambda (el) (equal? (item-id el) id))
     (obj-list-lst el-lst))
)

(define (add-sub-element sub-el el-lst)
  (set-element 
    (struct-copy obj-list el-lst
      [lst (cons sub-el (obj-list-lst el-lst))]
    )
  )
)

(define (remove-sub-element id el-lst)
  (set-element 
    (struct-copy obj-list el-lst
      [lst (remove (get-sub-element id el-lst) 
      (obj-list-lst el-lst))]
    )
  )
)

(define (set-sub-element sub-el el-lst)
  ;(displayln `("size 1:", (length (obj-list-lst (get-element "blocks")))))
  (remove-sub-element (item-id sub-el) el-lst)
  (add-sub-element sub-el (get-element "blocks"))
)

(define (test)
  (displayln "test")

  ;(displayln (generate-blocks-posns '() 2 *border-left *border-right))

  ; (for ([blk (obj-list-lst (get-element "blocks"))])
  ;   (displayln `(,(item-id blk), (block-hp blk)))
  ; )
  ; (displayln `("size:", (length (obj-list-lst (get-element "blocks")))))

)

(define (get-lists main)
  (filter (lambda (lst)
    (and (not (null? lst))(list? lst))) 
      main)
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
      (update-ball)
      (update-ball)
      (paint-callback this 'y)
    )

    (define game-timer
      (new timer%
           [notify-callback tick] [interval #f])
    )

    (define/public (init-game)
      (set-element (generate-blocks 1))
      (play-game)
    )

    (define/public (play-game)
      (send game-timer start FRAME-RATE)
    )

    (define/public (quit-game)
      (send game-timer stop)
    )

    (define/public (win-game)
      (send game-timer stop)
    )

    (define/public (lose-game)
      (send game-timer stop)
    )

    (define/public (paint-call)
      (paint-callback this 'y)
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
            [(equal? key #\space )
                  (test)
            ]
            [else (println key)]
        )

        ; Display of any changes
        ;(paint-callback this 'y)
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
          (handle-button (item-id btn)))
      )

      ; Display of any changes
      ;(paint-callback this 'y)
      )

    (define (paint-callback _self _evt)
      ; a dc object is a drawing context for drawing graphics and text
      (let ([dc (get-dc)])
        ; Clear old changes
        (send dc clear)

        (send dc set-background background-color)
        (send dc set-smoothing 'smoothed)

        ;; draw

        (for ([e (in-list (get-elements))])
            (draw-element dc e)
        )

        (for ([btn (in-list (get-buttons))])
            (draw-button dc btn)
        )
      )

      )

    (super-new [paint-callback paint-callback])
    ))

;;  ---------- Run ----------

; Create a custom-canvas object with frame parent
(define canvas (new custom-canvas% [parent f]))

; Show the game
(send f show #t)