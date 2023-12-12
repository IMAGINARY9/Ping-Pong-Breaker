#lang racket
(require racket/gui)
(require rackunit)
;(require test-engine/racket-tests)

; find "commented-tests"

;;  ---------- DEFINITIONS ----------

; Screen size constants to create gameboard
(define WIDTH 512)
(define HEIGHT 768)

; Size of the game cell
(define PIXEL 16)

; Game updating frame rate
;(define FRAME-RATE 17) ;60fps
;(define FRAME-RATE 8) ;120fps
(define FRAME-RATE 4) ;250fps
;(define FRAME-RATE 1) ;1000fps

; Fonts
(define font-title (make-object font% 64 'swiss))
(define font-button (make-object font% 28 'swiss))
(define font-default (make-object font% 12 'default))

; Brushes
(define brush-button  (new brush% [color (make-object color% 250 210 205)]))
(define brush-ball (new brush% [color (make-object color% 60 60 125)]))
(define brush-player (new brush% [color (make-object color% 250 210 205)]))
(define brush-destroyer (new brush% [color (make-object color% 250 210 205 0.25)]))
(define brush-wall (new brush% [color (make-object color% 240 140 135)]))
(define brush-block1 (new brush% [color (make-object color% 250 210 170)]))
(define brush-block2 (new brush% [color (make-object color% 250 160 135)]))
(define brush-block3 (new brush% [color (make-object color% 245 125 130)]))
(define brush-default (new brush% [color "white"][style 'opaque]))

; Color for background
(define background-color (make-object color% 160 85 120))


;;  ---------- DEFINITIONS END ----------

;;  ---------- TOOLS ----------

;; Data type
;; posn is a structure (posn Number Number)  
;; interpretation: a prefab struct representing a point in 2d space
(struct posn (x y) #:transparent)

; posn-add : posn posn -> posn
; add two positions together
; header: (define (posn-add posn1 posn2) posn)

;; Template:
; (define (posn-add posn1 posn2)
;   (posn ... (... posn1) ... (... posn2)))

;; Code
(define/match (posn-add posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (+ x1 x2) (+ y1 y2))]
)

(check-equal? (posn-add (posn 3 4) (posn 1 2)) (posn 4 6))
(check-equal? (posn-add (posn -1 0) (posn 1 -2)) (posn 0 -2))

; posn-sum : posn ... posn -> posn
; add two and more positions together
; header: (define (posn-sum posn . other) posn)

;; Template:
; (define (posn-sum posn . other)
;   (foldl ... (... posn1) ... (... other)))

;; Code
(define (posn-sum p . other)
  (foldl posn-add p other)
)

(check-equal? (posn-sum (posn 3 4) (posn 1 2) (posn 2 2)) (posn 6 8))
(check-equal? (posn-sum (posn -1 0) (posn 1 1) (posn 3 3)) (posn 3 4))

; posn-sum-lst : List<posn> -> posn
; add all posns in List together
; header: (define (posn-sum-lst posn-lst) posn)

;; Template:
; (define (posn-sum-lst posn-lst)
;   (foldl ... (posn 0 0) ... (... posn-lst)))

;; Code
(define (posn-sum-lst posn-lst)
  (foldl posn-add (posn 0 0) posn-lst)
)

(check-equal? (posn-sum-lst (list (posn 0 -2) (posn 1 2))) (posn 1 0))
(check-equal? (posn-sum-lst (list (posn 3 4) (posn 1 2) (posn 2 2))) (posn 6 8))

; posn-add-val : posn Number -> posn
; add position and value
; header: (define (posn-add-val p val) posn)

;; Template:
; (define (posn-add-val p val)
;   (posn ... (posn-x p) ... (posn-y p) ... (... val)))

;; Code
(define (posn-add-val p val)
  (posn (+ (posn-x p) val) 
        (+ (posn-y p) val)
))

(check-equal? (posn-add-val (posn 3 4) 3) (posn 6 7))
(check-equal? (posn-add-val (posn -1 0) 2) (posn 1 2))

; posn-x-add-val : posn Number -> posn
; add position-x and value
; header: (define (posn-x-add-val p val) posn)

;; Template:
; (define (posn-x-add-val p val)
;   (posn (... (posn-x p) ... val)) (posn-y p)))

;; Code
(define (posn-x-add-val p val)
  (posn (+ (posn-x p) val) (posn-y p)
))

(check-equal? (posn-x-add-val (posn 3 4) 3) (posn 6 4))
(check-equal? (posn-x-add-val (posn -1 0) 2) (posn 1 0))

; posn-y-add-val : posn Number -> posn
; add position-y and value
; header: (define (posn-y-add-val p val) posn)

;; Template:
; (define (posn-y-add-val p val)
;   (posn (posn-x p) (... (posn-y p) ... val))))

;; Code
(define (posn-y-add-val p val)
  (posn (posn-x p) (+ (posn-y p) val) 
))

(check-equal? (posn-y-add-val (posn 3 4) 3) (posn 3 7))
(check-equal? (posn-y-add-val (posn -1 0) 2) (posn -1 2))

; posn-substract : posn posn -> posn
; subtracts posn2 from posn1
; header: (define (posn-substract posn1 posn2) posn)

;; Template:
; (define (posn-substract posn1 posn2)
;   (posn ... (... posn1) ... (... posn2)))

;; Code
(define/match (posn-substract posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (- x1 x2) (- y1 y2))]
)

(check-equal? (posn-substract (posn 3 4) (posn 1 2)) (posn 2 2))
(check-equal? (posn-substract (posn -1 0) (posn 1 -2)) (posn -2 2))

; posn-multiply : posn posn -> posn
; multiplies posn1 and posn2 together
; header: (define (posn-multiply posn1 posn2) posn)

;; Template:
; (define (posn-multiply posn1 posn2)
;   (posn ... (... posn1) ... (... posn2)))

;; Code
(define/match (posn-multiply posn1 posn2)
  [((posn x1 y1) (posn x2 y2))
    (posn (* x1 x2) (* y1 y2))]
)

(check-equal? (posn-multiply (posn 3 4) (posn 2 2)) (posn 6 8))
(check-equal? (posn-multiply (posn -1 0) (posn 1 -2)) (posn -1 0))

; posn-product : posn ... posn -> posn
; multiplies two or more posns together
; header: (define (posn-product p . other) posn)

;; Template:
; (define (posn-product p . other)
;   (foldl ... (... p) ... (... other)))

;; Code
(define (posn-product p . other)
  (foldl posn-multiply p other)
)

(check-equal? (posn-product (posn 3 4) (posn 1 2) (posn 2 2)) (posn 6 16))
(check-equal? (posn-product (posn -1 0) (posn 1 1) (posn 3 3)) (posn -3 0))

; posn-multiply-val : posn Number -> posn
; multiplies posn by a value
; header: (define (posn-multiply-val p val) posn)

;; Template:
; (define (posn-multiply-val p val)
;   (posn ... (posn-x p) ... (posn-y p) ... (... val)))

;; Code
(define (posn-multiply-val p val)
  (posn (* (posn-x p) val) 
        (* (posn-y p) val)
))

(check-equal? (posn-multiply-val (posn 3 4) 2) (posn 6 8))
(check-equal? (posn-multiply-val (posn -1 0) 3) (posn -3 0))

; posn-normalize : posn -> posn
; normalizes the given posn
; header: (define (posn-normalize p) posn)

;; Template:
; (define (posn-normalize p)
;   (let ([l (sqrt ...)])
;     (posn ... (/ (posn-x p) l) ... (/ (posn-y p) l))))

;; Code
(define (posn-normalize p)
  (let ([l (sqrt 
      (+ (* (posn-x p) (posn-x p))
         (* (posn-y p) (posn-y p))))])
    (posn (/ (posn-x p) l) (/ (posn-y p) l))
  )
)

(check-equal? (posn-normalize (posn 3 4)) (posn (/ 3 5) (/ 4 5)))

; Border is a posn
(define BLOCKS-BORDER-LU (posn 1 (/ (/ HEIGHT PIXEL) 8)))
(define BLOCKS-BORDER-RD (posn (- (/ WIDTH PIXEL) 1) (/ (/ HEIGHT PIXEL) 2)))

; Max block size on game screen 
(define MAX-BLOCKS-NUMBER 
  (* (- (posn-y BLOCKS-BORDER-RD) (posn-y BLOCKS-BORDER-LU)) 
      (- (posn-x BLOCKS-BORDER-RD) (posn-x BLOCKS-BORDER-LU)))
)

;; Data type
;; item is a structure (item id)
;; where id is a Number
;; interpretation: a prefab struct representing item with some identifier
(struct item (id) #:transparent)

;; entity is a structure (entity id position size)
;; where id is a Number, position and size is a posn
;; interpretation: a prefab struct representing entity in application
(struct entity item (position size) #:transparent)

; check-hover : posn, entity -> Boolean
; does the point overlap with the entity
; header: (define (check-hover p entity)

;; Template:
; (define (check-hover p entity) 
;   (and (>= (posn-x p) ... entity) (<= (posn-x p) ... entity)
;         (>= (posn-y p) ... entity) (>= (posn-y p) ... entity))

;; Code
(define (check-hover p ent)
  (and 
    (>= (posn-x p) (posn-x (entity-position ent))) 
      (<= (posn-x p) (+ (posn-x (entity-position ent))
         (posn-x (entity-size ent))))
    (>= (posn-y p)(posn-y (entity-position ent))) 
      (<= (posn-y p) (+ (posn-y (entity-position ent))
         (posn-y (entity-size ent))))
  )
)

(check-equal? (check-hover (posn 150 150) 
  (entity "test-hover-entity" (posn 120 120) (posn 50 50))) #t)
(check-equal? (check-hover (posn 150 150) 
  (entity "test-hover-entity" (posn 200 100) (posn 50 50))) #f)

;; Data type
;; Element is an any game element:
;   entity, item, drawable ...

;; element-list is a structure (element-list id List<element>)
;; where id is a Number, List<obj> is a list with objs
;; interpretation: a prefab struct representing object in application
(struct element-list item (lst) #:transparent)

;; obj is a structure (obj id position size)
;; where id is a Number, position and size is a posn
;; interpretation: a prefab struct representing object in application
(struct colorful entity (brush) #:transparent)

;;  ---------- TOOLS END ----------

;;  ---------- BUTTONS ----------

;; Data type
;; button is a structure inherited from an entity (button id position size text)
;; where text is a String
;; interpretation: a prefab struct representing button in application
(struct button colorful (text font))

; draw-button : dc, b -> Void
; draw button on canvas
; header: (define (draw-button dc b)
; template: (define (draw-button dc b) (send dc ... b ...))
(define (draw-button dc b)
  (let* ([pos (entity-position b)]
            [sz (entity-size b)])

      (send dc set-brush (colorful-brush b))
      
      (if (button-font b)
        (send dc set-font (button-font b))
        (send dc set-font font-default)
      )
                
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
  )
))

;; draw function, impossible to write tests

;; draw function, dc is an instance of the dc<%> interface, 
;; as an argument to the paint-callback function,
;; impossible to write tests

; handle-button : button-id -> Void
; button handling by button id
; header: (define (handle-button button-id)

;; Template:
; (define (handle-button button-id) (cond ... button-id ...)

;; Code
(define (handle-button button-id)
  (cond
  [(or (eq? button-id "play-btn") 
      (eq? button-id "restart-btn"))
    (send canvas init-game)
  ]
  [(eq? button-id "exit-btn") 
    (send canvas quit-game)
  ]
  [(eq? button-id "pause-btn") 
    (send canvas pause-game)
  ]
  [(eq? button-id "continue-btn") 
    (send canvas continue-game)
  ]
  [(eq? button-id "quit-btn") (send f show #f)]
  [else (displayln button-id)]
  )
  (send canvas paint-call)
)

; commented-tests
;(check-equal? (void? (handle-button "exit-btn")) #f)

(define play-button (button "play-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 3)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Play" font-button))

(define quit-button (button "quit-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 2)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Quit" font-button))

(define restart-button (button "restart-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 3)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Restart" font-button))

(define continue-button (button "continue-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 3)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Continue" font-button))

(define exit-button (button "exit-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 2)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Exit" font-button))


;;  ---------- BUTTONS END ----------

;;  ---------- ENTITIES ----------

;; Data type
;; drawable is a structure inherited from an entity (drawable id position size form)
;; where form is a String
;; interpretation: a prefab struct representing game drawable that can be drawn by form
(struct drawable colorful (form))

;;  ---------- PLAYER ----------

;; player is an initial manipulable drawable, your actor
(define *player (drawable "player" 
  (posn (- (/ WIDTH 2) (* PIXEL 2)) (- HEIGHT (* PIXEL 4))) 
  (posn (* PIXEL 4) PIXEL) brush-player "round-rect"))

; on-board-by-width? : posn posn -> Boolean
; determine if a given position and size are within the board width
; header: (define (player-on-board? new-pos player-size) #f)

;; Template:
; (define (on-board-by-width? position size)
;    (and ... (posn-x position) ...
;       (+ (posn-x position) (posn-x size)) ...)))

;; Code
(define (on-board-by-width? position size)
  (and (> (posn-x position) 0)
       (< (+ (posn-x position) (posn-x size)) WIDTH))
)

(check-equal? (on-board-by-width? (posn 100 100) (posn 50 50)) #t)
(check-equal? (on-board-by-width? (posn 500 100) (posn 20 20)) #f)

; move-player : drawable Number -> Void
; move player on by x coord
; modify element in active state in state-box: *current-state
(define (move-player pl x)
  (let ([new-pos (posn-x-add-val (entity-position pl) x)])
    
    (when (on-board-by-width? new-pos (entity-size pl))
      (set-element 
        (struct-copy drawable pl
          [position #:parent entity new-pos])
      )
    )
))

;;  ---------- PLAYER END ----------

;;  ---------- DESTROYER ----------

;; destroyer is an initial drawable, when ball touch it game lose
(define *destroyer (drawable "destroyer" 
  (posn PIXEL (- HEIGHT (* PIXEL 4))) 
  (posn (- WIDTH (* PIXEL 2)) PIXEL) brush-destroyer "rect"))

; destroyer-collide? : Number -> Boolean
; if player lose the ball
; header: (define (destroyer-collide?) #f)

;; Template:
; (define (destroyer-collide?) ( ... (get-element "destroyer") ...)

;; Code
(define (destroyer-collide?)
  (and 
    (not (equal? (get-ball-cols (get-element "ball") 
      (get-element "destroyer") 1 90) (posn 0 0)))
    (equal? (get-ball-cols (get-element "ball") 
      (get-element "player") 1 90) (posn 0 0))
))

; impossible test before the game start

;;  ---------- DESTROYER END ----------

;;  ---------- BALL ----------

;; Data type
;; ball is a structure inherited from an drawable (ball id position size form direction)
;; where direction is a posn
;; interpretation: a prefab struct representing game ball
(struct ball drawable (direction))

;; *ball is a initial game ball
(define *ball (ball "ball"
  (posn (/ WIDTH 2) (/ HEIGHT 2))
  (posn PIXEL PIXEL) brush-ball
  "circle" (posn-normalize (posn 0 1))))

; reflect-direction : posn posn -> posn
; calculate reflection of current-direction
; header: (define (reflect-direction current-dir norm-new-dir) posn)

;; Template:
; (define (reflect-direction current-dir norm-new-dir)
;  (... current-dir ... norm-new-dir ...)

;; Code
(define (reflect-direction current-dir norm-new-dir)
  (posn-substract current-dir
      (posn-multiply-val (posn-product 
        current-dir norm-new-dir norm-new-dir) 2)
))

(check-equal? (reflect-direction (posn 3 4) (posn 1 0)) (posn -3 4))
(check-equal? (reflect-direction (posn 5 2) (posn 0 1)) (posn 5 -2))
(check-equal? (reflect-direction (posn 1 1) (posn 1 1)) (posn -1 -1))

;; Data type
; a Point is a posn
; a Degree is a Number
; interpretation: is a measurement of a plane angle

; get-circle-point : Number Degree Number Posn Degree -> Posn
; generate a point on a circle given the point number, angle, radius, center, and offset.

; Template:
; (define (get-circle-point n angl radius center offset)
;    (posn
;        (+ (posn-x center) ... n angl radius offset)
;        (+ (posn-y center) ... n angl radius offset)
; ))

;; Code
(define (get-circle-point n angl radius center offset)
  (posn
    (+ (posn-x center)(* radius (cos (degrees->radians (+ (* n angl) offset)))))
    (+ (posn-y center)(* radius (sin (degrees->radians (+ (* n angl) offset)))))
))

(check-equal? (get-circle-point 1 0 10 (posn 5 5) 0) (posn 15 5))

; get-circle-points : Number Degree Number Posn Degree -> List<posn>
; generate a list of points on a circle given the number of points, angle, radius, center, and offset.
; header: (define (get-circle-points n angl radius center offset) '())

;; Template:
; (define (get-circle-points n angl radius center offset)
;    (cond
;        [(<= n 0) ...) ; base case
;        [(cons  ; recursive case
;           (... n angl radius center offset) ...
;           (get-circle-points (... n ...) angl radius center offset)]
; ))

;; Code
(define (get-circle-points n angl radius center offset)
  (cond
    [(<= n 0) '()]
    [(cons (get-circle-point n angl radius center offset) 
      (get-circle-points (- n 1) angl radius center offset))
    ]
))

(check-equal? (get-circle-points 2 0 100 (posn 1 1) 0)
  (list (posn 101 1) (posn 101 1))
)

; get-point-cols : posn element-list List<entity> -> List<posn>
; catch all point collisions with list elements
; header: (define (get-point-cols point element lst) '())

;; Template:
; (define (get-point-cols point el-lst lst)
;    (cond
;        [(empty? lst) ...) ; base case
;        [... (first lst)  
;           (cons point (get-point-cols point el-lst (rest lst)))]
;        [else (cons point (get-point-cols point el-lst (rest lst)))]
; ))

;; Code
(define (get-point-cols point el-lst lst)
  (cond
    [(empty? lst) '()]
    [(check-hover point (first lst))
      (when (block? (first lst))
        (block-collide el-lst (first lst)))
      (cons point (get-point-cols point el-lst (rest lst)))
    ]
    [else (get-point-cols point el-lst (rest lst))]
))

(check-equal? (get-point-cols (posn 0 0) 
  (element-list "test-lst" '()) '()) '())

(check-equal? (get-point-cols (posn 150 550) 
  (element-list "test-lst" 
    (list (entity "test-ent1" (posn 40 40) (posn 50 50))
          (entity "test-ent2" (posn 0 0) (posn 20 20)))) 
  (list (entity "test-ent1" (posn 40 40) (posn 50 50))
          (entity "test-ent2" (posn 0 0) (posn 20 20)))) '())

; point-cols : posn element -> List<posn>
; return all point collisions with element or list elements
; header: (define (point-cols point element) '())

;; Template:
; (define (point-cols point element)
;    ((if (element-list? element) ...
;        (if ... element) (... point) ...)
; )

;; Code
(define (point-cols point element)
  (if (element-list? element)
    (get-point-cols point element (element-list-lst element))
    (if (check-hover point element)
      (cons point '()) '()
    )
))

(check-equal? (point-cols (posn 5 5) 
  (entity "test-element" (posn 0 0) (posn 10 10))) 
  (cons (posn 5 5) '()))
(check-equal? (point-cols (posn 5 5) 
  (element-list "test-lst" 
    (list (entity "test-ent1" (posn 40 40) (posn 50 50))
          (entity "test-ent2" (posn 50 0) (posn 20 20)))) 
  ) '())

; get-points-cols : List<posn> element -> posn
; return all points collisions with element 
; header: (define (get-points-cols points-list element) posn)

;; Template:
; (define (get-points-cols points-list element)
;    (cond
;        [(empty? points-list) (posn ...)] ; base case
;        [else (...  (first points-list) ; recursive case
;           (get-points-cols (rest points-list) element))] 
; )

;; Code
(define (get-points-cols points-list element)
  (cond
    [(empty? points-list) (posn 0 0)]
    [else (posn-sum 
      (posn-sum-lst (point-cols (first points-list) element))
      (get-points-cols (rest points-list) element))]
))

(check-equal? (get-points-cols '() #f) (posn 0 0))

; get-ball-cols : ball element Number [Number] -> posn
; return ball collisions with element 
; header: (define (get-ball-cols b element points-num [offset 0])

;; Template:
; (define (get-ball-cols b element points-num [offset 0]) ( ... )

;; Code
(define (get-ball-cols b element points-num [offset 0])
  (unless (or (false? b) (false? element))
    (let* ([angl (/ 360 points-num)]
            [radius (/ (posn-x (entity-size b)) 2)]
            [center (posn-add-val (entity-position b) radius)]
            [points (get-circle-points points-num angl radius center offset)]
            [res (get-points-cols points element)])

      (if (equal? res (posn 0 0)) (posn 0 0)
        (reflect-direction (ball-direction b) 
           (posn-normalize (posn-substract res center)))
      )
    )
))
; impossible test before the game start

; get-all-ball-colls : ball -> posn
; return ball collisions
; header: (define (get-all-ball-colls b)

;; Template:
; (define (get-all-ball-colls b) ( ... )

;; Code
(define (get-all-ball-colls b)
  (posn-sum
    (get-ball-cols b (get-element "player") 1 90)
    (get-ball-cols b (get-element "blocks") 16)
    (get-ball-cols b (get-element "enemies") 16)
    (get-ball-cols b (get-element "walls") 4)
))
; impossible test before the game start

; change-ball-direction : ball, new-dir -> Void
; set new value to direction of game ball
; modify element in active state in state-box: *current-state
(define (change-ball-direction b new-dir)
  (unless (equal? new-dir (posn 0 0))
    (set-element (struct-copy ball b
        [direction (posn-normalize
          (cond
            [(integer? (posn-x new-dir)) (posn-add new-dir (posn 0.1 0.5))]
            [(integer? (posn-y new-dir)) (posn-add new-dir (posn -0.5 -0.1))]
            [else new-dir]
          )
        )]
    ))
))
; impossible test before the game start

; move-ball : ball -> Void
; set new value to direction of game ball
; modify element in active state in state-box: *current-state
(define (move-ball b)
    (set-element (struct-copy ball b
          [position #:parent entity 
            (posn-add (entity-position b) 
            (ball-direction b))])
))

; update-ball : -> Void
; invoke all game ball functions
(define (update-ball)
    (move-ball (get-element "ball"))
    (change-ball-direction (get-element "ball")
     (get-all-ball-colls (get-element "ball")))
)

;;  ---------- BALL END ----------

;;  ---------- WALLS ----------

; *walls is an initial List<drawable>
(define *walls (element-list "walls" (list 
  (drawable "wall0" (posn 0 0) (posn PIXEL HEIGHT)
    brush-wall "rect") 
  (drawable "wall1" (posn (- WIDTH PIXEL) 0) (posn PIXEL HEIGHT) 
    brush-wall "rect")
  (drawable "wall2" (posn 0 0) (posn WIDTH PIXEL) 
    brush-wall "rect")
    )
))

;;  ---------- WALLS END ----------

;;  ---------- BLOCKS ----------

;; Data type
;; block is a structure inherited from an drawable (block id position size form hp)
;; where hp is a Number
;; interpretation: a prefab struct representing block
(struct block drawable (hp))

; *blocks is an initial List<block>
(define *blocks (element-list "blocks" (list 
  (block "block0" (posn 320 272) 
    (posn PIXEL PIXEL) brush-block1 "rect" 2) 
  (block "block1" (posn 304 288) 
    (posn PIXEL PIXEL) brush-block1 "rect" 2) 
  )
))

; generate-block-posn : posn posn -> posn
; generate random position in given borders
; header: (define (generate-block-posn border-l border-r)

;; Template:
; (define (generate-block-posn border-l border-r)
; (posn ... (random (posn-x ...) (posn-y ...)) ...))

;; Code
(define (generate-block-posn border-l border-r)
  (posn (* (random (posn-x border-l) (posn-x border-r)) PIXEL)
        (* (random (posn-y border-l) (posn-y border-r)) PIXEL)
))

; use random (impossible to make tests)
;(check-equal? (posn-x (generate-block-posn (posn 0 0) (posn 10 10))) (* 16 (random 0 10)))
;(check-equal? (posn-y (generate-block-posn (posn 5 5) (posn 15 15))) (* 16 (random 5 15)))

; generate-blocks-posns : List<empty> Number posn posn -> List<posn>
; generate List<posn> with unique positions
; header: (define (generate-blocks-posns acc n border-l border-r)

;; Template:
; (define (generate-blocks-posns acc n border-l border-r)
; (if (n ...) ... ; base case
;   (if (member ...)  ; recursive case
;     (generate-blocks-posns ...)
;     (generate-blocks-posns ...))) 

;; Code
(define (generate-blocks-posns acc n border-l border-r)
  (if (<= n 0) acc
    (let ([r-posn (generate-block-posn border-l border-r)])
      (if (member r-posn acc)
        (generate-blocks-posns acc n border-l border-r)
        (generate-blocks-posns (cons r-posn acc) (- n 1) border-l border-r)
      )
    )
))

(check-equal? (length (generate-blocks-posns '() 5 (posn 0 0) (posn 10 10))) 5)
(check-equal? (length (generate-blocks-posns '() 10 (posn 5 5) (posn 15 15))) 10)
(check-equal? (length (generate-blocks-posns '() 3 (posn 0 0) (posn 100 100))) 3)

; get-block-brush : Number -> brush%
; return brush depend on block hp
; header: (define (get-block-brush hp)

;; Template:
; (define (get-block-brush hp)
; (cond 
;   [(= hp ...) ...]
;   [else ...]  ; error case

;; Code
(define (get-block-brush hp)
  (cond
    [(= hp 1) brush-block1]
    [(= hp 2) brush-block2]
    [(= hp 3) brush-block3]
    [else brush-default]
  )
)

(check-equal? (get-block-brush 1) brush-block1)
(check-equal? (get-block-brush 0) brush-default)

; generate-blocks-list : List<posn> -> List<block>
; generate list of blocks
; header: (define (generate-blocks-list posn-lst)

;; Template:
; (define (generate-blocks-list posn-lst)
; (if (empty? posn-lst) ... ; base case
;   (cons ... (first posn-lst) ...  ; recursive case
;      (generate-blocks-list (rest posn-lst))))) 

;; Code
(define (generate-blocks-list posn-lst)
  (if (empty? posn-lst) '()
    (cons (let ([hp (random 1 4)]) 
      (block (~a "block" (length posn-lst)) 
        (first posn-lst) (posn PIXEL PIXEL) 
        (get-block-brush hp) "rect" hp))
      (generate-blocks-list (rest posn-lst)))
  )
)

(check-equal? (generate-blocks-list '()) '())
(check-equal? (length (generate-blocks-list 
  (list (posn 0 0) (posn 1 2)))) 2)

; generate-blocks : Number -> element-list
; generate element-list with list of n number of blocks
; header: (define (generate-blocks n)

;; Template:
; (define (generate-blocks n)
; (element-list "blocks" ...
;   (if (< n MAX-BLOCKS-NUMBER) n MAX-BLOCKS-NUMBER) ...)

;; Code
(define (generate-blocks n)
  (element-list "blocks" 
    (generate-blocks-list (generate-blocks-posns '() 
                (if (< n MAX-BLOCKS-NUMBER) n MAX-BLOCKS-NUMBER)
                   BLOCKS-BORDER-LU BLOCKS-BORDER-RD)
     )
))

(check-equal? (length (element-list-lst (generate-blocks 5))) 5)
(check-equal? (length (element-list-lst (generate-blocks 100000)))
   MAX-BLOCKS-NUMBER)

; block-collide : element-list block -> void
; processing colliding with block
; modify field elements in active state in state-box: *current-state 
(define (block-collide lst blk)
    (if (<= (block-hp blk) 1)
      (remove-sub-element (item-id blk) lst)
      (let ([new-hp (- (block-hp blk) 1)])
        (set-sub-element 
        (struct-copy block blk
          [hp new-hp]
          [brush #:parent colorful (get-block-brush new-hp)]
        ) lst)
      )
))
; impossible test before the game start

;;  ---------- BLOCKS END ----------

;;  ---------- ENEMIES ----------

;; Data type
;; enemy is a block
;; where hp is a Number
;; interpretation: a prefab struct representing block

; *enemies is an initial List<block>
(define *enemies (element-list "enemies" (list 
  (block "enemy0" (posn (- (/ WIDTH 2) (/ PIXEL 2)) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy1" (posn (- (* WIDTH 0.25) PIXEL) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy2" (posn (* WIDTH 0.75) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  )
))

; enemies-over? : -> Boolean
; is list with enemies is empty?
; header: (define (chenemies-over?eck-enemies) #f)

;; Template:
; (define (enemies-over?)
; (when (... (get-element "enemies")) ...)
(define (enemies-over?)
  (empty? (element-list-lst (get-element "enemies")))
)
; impossible test before the game start

;;  ---------- ENEMIES END ----------

; draw-rectangle : dc, o -> Void
; draw o - object rectangle form on canvas
; header: (define (draw-rectangle dc o)
; template: (define (draw-rectangle dc o) (send dc draw-rectangle ... o ...))
(define (draw-rectangle dc o)
    (let* ([pos (entity-position o)]
            [sz (entity-size o)])
                
      (send dc set-brush (colorful-brush o))

      (send dc draw-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
))

;; draw function, impossible to write tests

; draw-rounded-rectangle : dc, o -> Void
; draw o - object rounded rectangle form on canvas
; header: (define (draw-rounded-rectangle dc o)
; template: (define (draw-rounded-rectangle dc o) (send dc draw-rounded-rectangle ... o ...))
(define (draw-rounded-rectangle dc o)
    (let* ([pos (entity-position o)]
            [sz (entity-size o)])
                
      (send dc set-brush (colorful-brush o))

      (send dc draw-rounded-rectangle
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz)
        5
      )
))

;; draw function, impossible to write tests

; draw-circle : dc, o -> Void
; draw o - object circle form on canvas
; header: (define (draw-circle dc o)
; template: (define (draw-circle dc o) (send dc draw-ellipse ... o ...))
(define (draw-circle dc o)
    (let* ([pos (entity-position o)]
            [sz (entity-size o)])

      (send dc set-brush (colorful-brush o))
                
      (send dc draw-ellipse
        (posn-x pos)
        (posn-y pos)
        (posn-x sz)
        (posn-y sz))
    ) 
)

;; draw function, impossible to write tests

; draw-form : dc, el -> Void
; call the function depend on element form
; header: (define (draw-form dc el)

;; Template: 
; (define (draw-form dc el) 
;   [(eq? (drawable-form el) "rect") ... ]
;   [(eq? (drawable-form el) "round-rect") ... ]
;   [(eq? (drawable-form el) "circle") ... ]
;   [else ...] ; error case

;; Code
(define (draw-form dc el)
  (if (drawable? el)
      (cond
        [(eq? (drawable-form el) "rect")
         (draw-rectangle dc el)]
        [(eq? (drawable-form el) "round-rect")
         (draw-rounded-rectangle dc el)]
        [(eq? (drawable-form el) "circle")
         (draw-circle dc el)]
        [else (displayln `(,(item-id el) ,(drawable-form el)))])
      (displayln `("undrawable element:", (item-id el)))
))

;; draw function, impossible to write tests

; draw-element : dc, el -> Void
; call the function depend on element type
; header: (define (draw-element dc el)

;; Template: 
; (define (draw-element dc el) 
;   (cond [(element-list? el) ... ]
;         [else ...]))

;; Code
(define (draw-element dc el) 
  (cond
      [(element-list? el)
        (for ([sub-el (in-list (element-list-lst el))]) 
          (draw-form dc sub-el))
      ]
      [else (draw-form dc el)]
))

;; draw function, impossible to write tests

;;  ---------- ENTITIES END ----------

;;  ---------- TITLES END ----------

;; Data type
;; title is a structure inherited from an entity (title id position text font)
;; where text is a String, font is a font%
;; interpretation: a prefab struct representing title in application
(struct title entity (text font))

(define menu-up-title 
  (title "menu-up-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 5)) 
    (posn 0 0) "Ping-Pong" font-title))

(define menu-down-title1 
  (title "menu-down-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 1.4)) 
    (posn 0 0) "B   E   K   R" font-title))

(define menu-down-title2
  (title "menu-down-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 1.45)) 
    (posn 0 0) "   R   A   E   " font-title))

(define pause-title 
  (title "pause-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 5)) 
    (posn 0 0) "Pause" font-title))

(define win-title 
  (title "win-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 5)) 
    (posn 0 0) "You Win!!!" font-title))

(define lose-title 
  (title "lose-ttl" 
    (posn (/ WIDTH 2) (/ HEIGHT 5)) 
    (posn 0 0) "You Lose :(" font-title))

; draw-title : dc, t -> Void
; draw title on canvas
; header: (define (draw-title dc t)
; template: (define (draw-title dc t) (send dc ... t ...))
(define (draw-title dc t)
  (let* ([pos (entity-position t)])
      
      (if (title-font t)
        (send dc set-font (title-font t))
        (send dc set-font font-default)
      )

  (define-values (text-w text-h d a)
    (send dc get-text-extent (title-text t)))
  (send dc draw-text 
    (title-text t) 
    (+ (posn-x pos) (/ (- text-w) 2))
    (+ (posn-y pos) (/ (- text-h) 2))
  )
))

;; draw function, impossible to write tests

;;  ---------- TITLES END ----------

;;  ---------- STATES ----------

;; Data type
;; state is a structure (state elements buttons titles)
;; where elements is a List<Element>,
; buttons is a List<button>, titles is a List<title>
;; interpretation: a prefab struct representing world-state  
(struct state (elements buttons titles))

(define menu-state (state '() (list play-button quit-button) 
  (list menu-up-title menu-down-title1 menu-down-title2)))
(define pause-state (state '() (list continue-button exit-button) 
  (list pause-title)))
(define win-state (state '() (list restart-button exit-button)
  (list win-title)))
(define lose-state (state '() (list restart-button exit-button) 
  (list lose-title)))
(define game-state (state (list *player *destroyer *ball *blocks *walls *enemies) '() '()))

;; Data type
;; state-box is a structure (state active-state)
;; where active-state is a state
;; interpretation: a prefab struct contain active world-state
(struct state-box (active-state))

; *current-state is a current world-state 
(define *current-state (state-box menu-state))

; set-state : state -> Void
; set new current state
; modify state-box: *current-state
(define (set-state state)
 (set! *current-state (state-box state)))

; get-elements : -> List<Element>
; get list of elements from current state
; read state-box: *current-state
(define (get-elements)
  (state-elements (state-box-active-state *current-state)))

(check-equal? (get-elements) '())

; set-elements : List<Element> -> Void
; set new elements to current state
; modify state in state-box: *current-state
(define (set-elements elems)
  (set-state (state elems (get-buttons) (get-titles))))

; get-buttons : -> List<Button>
; get list of buttons from current state
; read state-box: *current-state
(define (get-buttons)
  (state-buttons (state-box-active-state *current-state)))

(check-equal? (get-buttons) (list play-button quit-button))

; set-buttons : List<Button> -> Void
; set new buttons to current state
; modify state in state-box: *current-state
(define (set-buttons buttons)
  (set-state (state (get-elements) buttons (get-titles))))

; get-titles : -> List<Title>
; get list of titles from current state
; read state-box: *current-state
(define (get-titles)
  (state-titles (state-box-active-state *current-state)))

(check-equal? (get-titles) 
  (list menu-up-title menu-down-title1 menu-down-title2))

; set-titles : List<Title> -> Void
; set new titles to current state
; modify state in state-box: *current-state
(define (set-titles titles)
  (set-state (state (get-elements) (get-buttons) titles)))

; get-element : String -> Element
; get element by id
; header: (define (get-element id)
; template: (define (get-element id) (findf (lambda (...) ... id ...) ...)
(define (get-element id) (findf 
  (lambda (el) (equal? (item-id el) id))
     (get-elements)
))

(check-equal? (get-element "noname") #f)

; add-element : Element -> Void
; add new element to state-elements of current state 
; modify field elements in active state in state-box: *current-state
(define (add-element el)
  (set-elements (cons el (get-elements))))

; commented-tests
;(check-equal? (begin (add-element (item "test-element"))
;                    (get-element "test-element")) (item "test-element"))

; remove-element : String -> Void
; remove element by id
; modify field elements in active state in state-box: *current-state
(define (remove-element id)
  (set-elements (remove (get-element id) (get-elements))))

; commented-tests
;(check-equal? (begin (remove-element "test-element")
;                    (get-element "test-element")) #f)

; set-element : Element -> Void
; replace element with the same id 
; modify field elements in active state in state-box: *current-state
(define (set-element el)
  (remove-element (item-id el))
  (add-element el)
)

; get-sub-element : String, List<Element> -> Element
; get element from given element-list by id 
; header: (define (get-sub-element id el-lst)
(define (get-sub-element id el-lst)(findf 
  (lambda (el) (equal? (item-id el) id))
     (element-list-lst el-lst))
)

(check-equal? (get-sub-element "sub-test-el1"
  (element-list "test-lst" 
    (list (entity "sub-test-el1" (posn 1 2) (posn 3 4))
          (entity "sub-test-el2" (posn 2 3) (posn 4 5)))))
 (entity "sub-test-el1" (posn 1 2) (posn 3 4)))

; add-sub-element : Element, List<Element> -> Void
; add sub-element to the given element-list
; modify field elements in active state in state-box: *current-state
(define (add-sub-element sub-el el-lst)
  (set-element 
    (struct-copy element-list el-lst
      [lst (cons sub-el (element-list-lst el-lst))]
    )
))
; commented-tests
; (check-equal? (begin
;                 (add-element (element-list "test-element-list"
;                                            (list (item "test-sub-item1")
;                                                  (item "test-sub-item2"))))
;                 (add-sub-element (item "test-sub-item3")
;                                  (element-list "test-element-list"
;                                            (list (item "test-sub-item1")
;                                                  (item "test-sub-item2"))))
;                     (get-element "test-element-list"))
;                                  (element-list "test-element-list"
;                                            (list (item "test-sub-item3")
;                                                  (item "test-sub-item1")
;                                                  (item "test-sub-item2"))))

; remove-sub-element : Element, List<Element> -> Void
; remove sub-element to the given element-list
; modify field elements in active state in state-box: *current-state
(define (remove-sub-element id el-lst)
  (set-element 
    (struct-copy element-list el-lst
      [lst (remove (get-sub-element id el-lst) 
      (element-list-lst el-lst))]
    )
))
; commented-tests
; (check-equal? (begin
;                 (remove-sub-element "test-sub-item2"
;                                     (element-list "test-element-list"
;                                            (list (item "test-sub-item1")
;                                                  (item "test-sub-item2"))))
;                     (get-element "test-element-list"))
;                                  (element-list "test-element-list"
;                                            (list (item "test-sub-item1"))))
; (remove-element "test-element-list")

; set-sub-element : Element, List<Element> -> Void
; replace sub-element to the given element-list
; modify field elements in active state in state-box: *current-state
(define (set-sub-element sub-el el-lst)
  (remove-sub-element (item-id sub-el) el-lst)
  (add-sub-element sub-el (get-element (item-id el-lst)))
)
; refactor

;;  ---------- STATES END ----------

;;  ---------- Game Logic ----------

; f is a Frame
(define f (new frame% [label "PPB"]
               [width WIDTH] [height HEIGHT]
               [border 0]))

; Custom canvas is a Class
; Overridden canvas% class
(define custom-canvas%
  (class canvas%
    (inherit get-dc)

    ; tick : -> void
    ; game timer tick, game update 
    (define (tick)
      (update-ball)
      (update-ball)
      (when (enemies-over?) (send canvas win-game))
      (when (destroyer-collide?) (send canvas lose-game))
      (paint-callback this 'y)
    )

    ; *in-game is Boolean variable that indicate if the game active
    (define *in-game #f)

    ; game-timer is a timer%
    (define game-timer
      (new timer% [notify-callback tick] [interval #f]))

    ; init-game : -> void
    ; initialize a gameboard, start game
    ; modify state-box: *current-state
    (define/public (init-game)
      (set-state game-state)
      (set-element (generate-blocks 100))
      (play-game)
    )

    ; play-game : -> void
    ; set variable to been in game and start game timer
    ; modify variable *in-game
    (define/private (play-game)
      (set! *in-game #t)
      (send game-timer start FRAME-RATE)
    )

    ; stop-game : -> void
    ; set variable not to been in game and stop game timer
    ; modify variable *in-game
    (define/private (stop-game)
      (set! *in-game #f)
      (send game-timer stop)
    )
  
    ; quit-game : -> void
    ; quit from the game
    ; modify state-box: *current-state
    (define/public (quit-game)
      (set-state menu-state)
      (stop-game)
    )

    ; pause-game : -> void
    ; pause the game (change game state to pause-state), call stop-game
    ; read field state-elements from state-box: *current-state
    ; modify state-box: *current-state
    (define/public (pause-game)
      (let ([elems (get-elements)])
        (set-state pause-state)
        (set-elements elems)
      )
      (stop-game)
    )

    ; continue-game : -> void
    ; continue the game (change game state to game-state)
    ; read field state-elements from state-box: *current-state
    ; modify state-box: *current-state
    (define/public (continue-game)
      (let ([elems (get-elements)])
        (set-state game-state)
        (set-elements elems)
      )
      (play-game)
    )

    ; win-game : -> void
    ; change game state to win-state, call stop-game
    ; modify state-box: *current-state
    (define/public (win-game)
      (set-state win-state)
      (stop-game)
    )
    
    ; lose-game : -> void
    ; change game state to lose-game, call stop-game
    ; modify state-box: *current-state
    (define/public (lose-game)
      (set-state lose-state)
      (stop-game)
    )

    ; paint-call : -> void
    ; safety invoke paint callback
    (define/public (paint-call)
      (paint-callback this 'y)
    )

    ; on-char : key-event -> void
    ; Overridden on-char function for checking custom keystrokes on the keyboard
    (define/override (on-char key-event)
        (define key (send key-event get-key-code))
        
        (when (not (false? *in-game)) (cond
            ; base case any key-release has do nothing
            [(equal? key 'release) ]
            ; Left arrow or 'a' key moves the player left
            [(or (equal? key 'left)
                (equal? key #\a ))
                    (move-player (get-element "player") (- PIXEL))
            ]
            ; Right arrow or 'd' key moves the player right
            [(or (equal? key 'right)
                (equal? key #\d ))
                    (move-player (get-element "player") PIXEL)
            ]
            ; Escape key pause the game
            [(equal? key 'escape)
                (pause-game)
                (paint-callback this 'y)
            ]
            [else (displayln key)]
        ))
    )

    ; on-event : evt -> void
    ; Overridden on-event function for checking mouse events
    (define/override (on-event evt)
      (define type (send evt get-event-type))
        
        ; check buttons
        (for/list ([btn (get-buttons)])
            (when (and (eq? 'left-down type) 
              (check-hover (posn (send evt get-x) (send evt get-y)) btn)) 
            (handle-button (item-id btn)))
        )
    )

    ; paint-callback
    ; Overridden on-event function for draw all changes
    (define (paint-callback _self _evt)
      ; a dc object is a drawing context for drawing graphics and text
      (let ([dc (get-dc)])
        ; Clear old changes
        (send dc clear)
        ; Set background
        (send dc set-background background-color)
        ; Set smoothing
        (send dc set-smoothing 'smoothed)

        ; Draw elements
        (for/list ([el (get-elements)])
            (draw-element dc el)
        )

        ; Draw buttons
        (for/list ([btn (get-buttons)])
            (draw-button dc btn)
        )

        ; Draw titles
        (for/list ([ttl (get-titles)])
            (draw-title dc ttl)
        )
      )
    )
    ; draw function, impossible to write tests

    ; invokes on create new canvas%
    (super-new [paint-callback paint-callback])
  ))

;;  ---------- Run ----------

; Create a custom-canvas object with frame parent
(define canvas (new custom-canvas% [parent f]))

; Show the game
(send f show #t)

;(test)