#lang racket
(require racket/gui)
(require rackunit)
;(require test-engine/racket-tests)

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

; posn-add-val : List<posn> -> posn
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
) ;30x8 ;240

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

;; entity-list is a structure (entity-list id List<obj>)
;; where id is a Number, List<obj> is a list with objs
;; interpretation: a prefab struct representing object in application
(struct entity-list item (lst) #:transparent)

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

; impossible to write a tests

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
  [else (display button-id)]
  )
  (send canvas paint-call)
)

;(check-equal? (handle-button "play-btn") (send canvas init-game))

(define play-button (button "play-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 3)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Play" font-button))

(define quit-button (button "quit-btn" 
  (posn (- (/ WIDTH 2)(/ WIDTH 3 2)) (/ HEIGHT 2)) 
  (posn (/ WIDTH 3) (/ HEIGHT 16)) brush-button 
  "Exit" font-button))

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

; move-player : Number -> Void
; move player on by x coord
; header: (define (move-player x) void)

;; Template:
; (define (move-player x) (... (get-element "player") ... x ...))

;; Code
(define (move-player x)
  (let ([pl (get-element "player")])
  (let ([new-pos (posn 
        (+ (posn-x (entity-position pl)) x)
        (posn-y (entity-position pl)))])
        
    (when (and (> (posn-x new-pos) 0) 
            (< (+ (posn-x new-pos) 
                  (posn-x (entity-size pl))) 
              WIDTH))

      (set-element 
        (struct-copy drawable pl
          [position #:parent entity new-pos])
      )
    )
  ))
)

; later
;(check-equal?)

;;  ---------- PLAYER END ----------

;;  ---------- DESTROYER ----------

;; destroyer is an initial drawable, when ball touch it game lose
(define *destroyer (drawable "destroyer" 
  (posn PIXEL (- HEIGHT (* PIXEL 4))) 
  (posn (- WIDTH (* PIXEL 2)) PIXEL) brush-destroyer "rect"))

; check-destroyer : Number -> Void
; check lose condition when true -> lose game
; header: (define (check-destroyer) void)

;; Template:
; (define (check-destroyer) (when ... (get-element "destroyer") ...)

;; Code
(define (check-destroyer)
  (when (and 
    (not (equal? (get-ball-cols (get-element "ball") 
      (get-element "destroyer") 1 90) (posn 0 0)))
    (equal? (get-ball-cols (get-element "ball") 
      (get-element "player") 1 90) (posn 0 0)))

    (send canvas lose-game)
  )
)

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
  (if (entity-list? el)
    (get-point-cols p el (entity-list-lst el))
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
            [radius (/ (posn-x (entity-size b)) 2)]
            [center (posn-add-val (entity-position b) radius)]
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
          [position #:parent entity 
            (posn-add (entity-position b) 
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

; *walls is an initial List<drawable>
(define *walls (entity-list "walls" (list 
  (drawable "wall0" (posn 0 0) (posn PIXEL HEIGHT)
    brush-wall "rect") 
  (drawable "wall1" (posn (- WIDTH PIXEL) 0) (posn PIXEL HEIGHT) 
    brush-wall "rect")
  (drawable "wall2" (posn 0 0) (posn WIDTH PIXEL) 
    brush-wall "rect")
  ; (drawable "wall3" (posn 0 (- HEIGHT PIXEL PIXEL)) (posn WIDTH PIXEL) 
  ;   brush-wall "rect")
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
(define *blocks (entity-list "blocks" (list 
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
;(check-equal? (posn-x (generate-block-posn (posn 0 0) (posn 100 100))) (* 16 (random 0 100)))


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
  )
)

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

; generate-blocks : Number -> entity-list
; generate entity-list with list of n number of blocks
; header: (define (generate-blocks n)

;; Template:
; (define (generate-blocks n)
; (entity-list "blocks" ...
;   (if (< n MAX-BLOCKS-NUMBER) n MAX-BLOCKS-NUMBER) ...)

;; Code
(define (generate-blocks n)
  (entity-list "blocks" (generate-blocks-list 
    (generate-blocks-posns '() 
      (if (< n MAX-BLOCKS-NUMBER) n MAX-BLOCKS-NUMBER)
     BLOCKS-BORDER-LU BLOCKS-BORDER-RD))
  )
)

(check-equal? (length (entity-list-lst (generate-blocks 5))) 5)
(check-equal? (length (entity-list-lst (generate-blocks 100000)))
   MAX-BLOCKS-NUMBER)

; block-collide : entity-list block -> void
; processing colliding with block
; header: (define (block-collide lst blk)

;; Template:
; (define (block-collide lst blk)
; (if (<= (block-hp blk) 1)
;  (... blk ...)
;  (... (- (block-hp blk) 1) ... blk ...)

;; Code
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
    )
)

; later
; (check-equal? ( (block-collide 
;   (entity-list "test-blocks" 
;     (list (block "testblk1" (posn 0 0) (posn 0 0) "rect" brush-default 1)
;     (block "testblk2" (posn 0 0) (posn 0 0) "rect" brush-default 1))
;   ) 
;   (block "testblk1" (posn 0 0) (posn 0 0) "rect" brush-default 1))) 1)

;;  ---------- BLOCKS END ----------

;;  ---------- ENEMIES ----------

;; Data type
;; enemy is a block
;; where hp is a Number
;; interpretation: a prefab struct representing block

; *enemies is an initial List<block>
(define *enemies (entity-list "enemies" (list 
  (block "enemy0" (posn (- (/ WIDTH 2) (/ PIXEL 2)) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy1" (posn (- (* WIDTH 0.25) PIXEL) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  (block "enemy2" (posn (* WIDTH 0.75) (/ HEIGHT 16)) 
    (posn PIXEL PIXEL) brush-ball "round-rect" 1)
  )
))

; check-enemies : -> void
; check win condition when true -> win game
; header: (define (check-enemies)

;; Template:
; (define (check-enemies)
; (when (... (get-element "enemies")) ...)
(define (check-enemies)
  (when (<= (length 
      (entity-list-lst (get-element "enemies"))) 0)
    (send canvas win-game))
)

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
  (cond
    [(eq? (drawable-form el) "rect")
        (draw-rectangle dc el)]
    [(eq? (drawable-form el) "round-rect")
        (draw-rounded-rectangle dc el)]
    [(eq? (drawable-form el) "circle")
        (draw-circle dc el)]
    [else (displayln `(,(item-id el) ,(drawable-form el)))]
))

; draw-element : dc, el -> Void
; call the function depend on element type
; header: (define (draw-element dc el)

;; Template: 
; (define (draw-element dc el) 
;   (cond [(entity-list? el) ... ]
;         [else ...]))

;; Code
(define (draw-element dc el) 
  (cond
      [(entity-list? el)
        (for ([sub-el (in-list (entity-list-lst el))]) 
          (draw-form dc sub-el))
      ]
      [else (draw-form dc el)]
))

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
)
)

;;  ---------- TITLES END ----------

;;  ---------- STATES ----------

;; Data type
;; element is an any game element:
;   entity, item, drawable ...

;; Data type
;; state is a structure (state elements buttons titles)
;; where elements is a List<element>,
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

; set-state : dc, el -> Void
; set new current state
; modify state *current-state
; header: (define (set-state state)
; template: (define (set-state state) (set! ... state ...)
(define (set-state state)
 (set! *current-state (state-box state)))

; get-elements : -> List<Elements>
; get list of current elements
; header: (define (get-elements)
; template: (define (get-elements) (state-elements ...)
(define (get-elements)
  (state-elements (state-box-active-state *current-state)))

(check-equal? (get-elements) '())

; set-elements : List<Elements> -> Void
; set new elements to current state
; header: (define (set-elements elems)
; template: (define (set-elements elems) (... (state elems ...))
(define (set-elements elems)
  (set-state (state elems (get-buttons) (get-titles))))

; get-buttons : -> List<Buttons>
; get list of current buttons
; header: (define (get-buttons)
; template: (define (get-buttons) (state-buttons ...)
(define (get-buttons)
  (state-buttons (state-box-active-state *current-state)))

(check-equal? (get-buttons) (list play-button quit-button))

; set-buttons : List<Buttons> -> Void
; set new buttons to current state
; header: (define (set-buttons buttons)
; template: (define (set-buttons buttons) (... (state ... buttons ...))
(define (set-buttons buttons)
  (set-state (state (get-elements) buttons (get-titles))))

; get-titles : -> List<titles>
; get list of current titles
; header: (define (get-titles)
; template: (define (get-titles) (state-titles ...)
(define (get-titles)
  (state-titles (state-box-active-state *current-state)))

(check-equal? (get-titles) 
  (list menu-up-title menu-down-title1 menu-down-title2))

; set-titles : List<titles> -> Void
; set new titles to current state
; header: (define (set-titles titles)
; template: (define (set-titles titles) (... (state ... titles))
(define (set-titles titles)
  (set-state (state (get-elements) (get-buttons) titles)))

; get-element : String -> entity
; get element by id
; header: (define (get-element id)
; template: (define (get-element id) 
;   (findf (lambda (...) ... id ...) ...)
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


; get-element : String -> entity
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

; set-element : entity -> Void
; remove element by id
; header: (define (set-element id)
; template: (define (set-element id) (remove (...) ...)
(define (set-element el)
  (remove-element (item-id el))
  (add-element el)
)

; get-sub-element : String, List<entity> -> entity
; get element from lists by id and position
; header: (define (get-sub-element id el-lst)
(define (get-sub-element id el-lst)(findf 
  (lambda (el) (equal? (item-id el) id))
     (entity-list-lst el-lst))
)

(define (add-sub-element sub-el el-lst)
  (set-element 
    (struct-copy entity-list el-lst
      [lst (cons sub-el (entity-list-lst el-lst))]
    )
  )
)

(define (remove-sub-element id el-lst)
  (set-element 
    (struct-copy entity-list el-lst
      [lst (remove (get-sub-element id el-lst) 
      (entity-list-lst el-lst))]
    )
  )
)

(define (set-sub-element sub-el el-lst)
  (remove-sub-element (item-id sub-el) el-lst)
  (add-sub-element sub-el (get-element "blocks"))
)

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
    ; game timer tick 
    ; header: (define (tick)

    ;; Template:
    ; (define (tick) (...)

    ;; Code
    (define (tick)
      (update-ball)
      (update-ball)
      (check-enemies)
      (check-destroyer)
      (paint-callback this 'y)
    )

    ; *in-game is Boolean variable that indicate if the game active
    (define *in-game #f)

    ; game-timer is a timer%
    (define game-timer
      (new timer%
           [notify-callback tick] [interval #f])
    )

    ; init-game : -> void
    ; initialize a gameboard
    ; header: (define/public (init-game)

    ;; Template:
    ; (define/public (init-game) (...)

    ;; Code
    (define/public (init-game)
      (set-state game-state)
      (set-element (generate-blocks 100))
      (play-game)
    )

    ; play-game : -> void
    ; start playing the game
    ; header: (define/private (play-game)

    ;; Template:
    ; (define/private (play-game) (...)

    ;; Code
    (define/private (play-game)
      (set! *in-game #t)
      (send game-timer start FRAME-RATE)
    )

    ; stop-game : -> void
    ; stop the game
    ; header: (define/private (stop-game)

    ;; Template:
    ; (define/private (stop-game) (...)

    ;; Code
    (define/private (stop-game)
      (set! *in-game #f)
      (send game-timer stop)
    )
  
    ; quit-game : -> void
    ; quit from the game
    ; header: (define/public (quit-game)

    ;; Template:
    ; (define/public (quit-game) (...)

    ;; Code
    (define/public (quit-game)
      (set-state menu-state)
      (stop-game)
    )

    ; pause-game : -> void
    ; pause the game (change game state to pause-state)
    ; header: (define/public (pause-game)

    ;; Template:
    ; (define/public (pause-game) (...)

    ;; Code
    (define/public (pause-game)
      (let ([elems (get-elements)])
        (set-state pause-state)
        (set-elements elems)
      )
      (stop-game)
    )

    ; continue-game : -> void
    ; continue the game (change game state to game-state)
    ; header: (define/public (continue-game)

    ;; Template:
    ; (define/public (continue-game) (...)

    ;; Code
    (define/public (continue-game)
      (let ([elems (get-elements)])
        (set-state game-state)
        (set-elements elems)
      )
      (play-game)
    )

    ; win-game : -> void
    ; change game state to win-state
    ; header: (define/public (win-game)

    ;; Template:
    ; (define/public (win-game) (...)

    ;; Code
    (define/public (win-game)
      (set-state win-state)
      (stop-game)
    )
    
    ; lose-game : -> void
    ; change game state to lose-game
    ; header: (define/public (lose-game)

    ;; Template:
    ; (define/public (lose-game) (...)

    ;; Code
    (define/public (lose-game)
      (set-state lose-state)
      (stop-game)
    )

    ; paint-call : -> void
    ; invoke paint callback
    ; header: (define/public (paint-call)

    ;; Template:
    ; (define/public (paint-call) (...)

    ;; Code
    (define/public (paint-call)
      (paint-callback this 'y)
    )

    ; on-char : key-event -> void
    ; Overridden on-char function for checking custom keystrokes on the keyboard 
    ; header: (define/override (on-char key-event)

    ;; Template:
    ; (define/override (on-char key-event) 
    ;   ( ... (send key-event get-key-code) ... )

    ;; Code
    (define/override (on-char key-event)
        (define key (send key-event get-key-code))
        
        (when (not (false? *in-game)) (cond
            ; base case any key-release has do nothing
            [(equal? key 'release) ]
            ; Left arrow or 'a' key moves the player left
            [(or (equal? key 'left)
                (equal? key #\a ))
                    (move-player (- PIXEL))
            ]
            ; Right arrow or 'd' key moves the player right
            [(or (equal? key 'right)
                (equal? key #\d ))
                    (move-player PIXEL)
            ]
            [(equal? key 'escape)
                (pause-game)
                (paint-callback this 'y)
            ]
            [else (displayln key)]
        ))
    )

    ; on-event : evt -> void
    ; Overridden on-event function for checking custom keystrokes on the keyboard 
    ; header: (define/override (on-event evt)

    ;; Template:
    ; (define/override (on-event evt) 
    ;   ( ... (send evt get-key-type) ... )

    ;; Code
    (define/override (on-event evt)
      (define type (send evt get-event-type))

        (for ([btn (in-list (get-buttons))])
            (when (and (eq? 'left-down type) 
              (check-hover (posn (send evt get-x) (send evt get-y)) btn)) 
            (handle-button (item-id btn)))
        )
      )

    ; paint-callback
    ; Overridden on-event function for draw all changes
    ; header: (define (paint-callback _self _evt)

    ;; Template:
    ; (define (paint-callback _self _evt)
    ;   ( ... (send evt get-key-type) ... )

    ;; Code
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
        (for ([el (in-list (get-elements))])
            (draw-element dc el)
        )

        ; Draw buttons
        (for ([btn (in-list (get-buttons))])
            (draw-button dc btn)
        )

        ; Draw titles
        (for ([ttl (in-list (get-titles))])
            (draw-title dc ttl)
        )
      )
    )

    ; invokes on create new canvas%
    (super-new [paint-callback paint-callback])
    ))

;;  ---------- Run ----------

; Create a custom-canvas object with frame parent
(define canvas (new custom-canvas% [parent f]))

; Show the game
(send f show #t)

;(test)