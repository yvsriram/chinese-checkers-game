#lang racket
(require "minimax.rkt")
(require "board_utils.rkt")
(require 2htdp/universe)
(require 2htdp/image)
(require lang/posn)

(define unit-size (* (sqrt 2) 21))
(define n 30)
(define n-players 2)
(define player-colors (list 'red 'green 'orange 'blue 'black))
(define player-next-colors (list 'LightPink 'GreenYellow 'Gold 'SkyBlue 'DarkGray))
(define pegs-per-player 10)
(define slot-radius 9)
(define board 1)
(define x-size 800)
(define y-size 600)
(define posns_board null)
(define full-board null)
(define empty-slots null)
(define empty-board null)
(define peg (rhombus unit-size 90 "solid" "white"))
(define initial-board null)
(define players-posns null)
(define mode 0)
(define unit1 peg)
(define unit2 peg)
(define posns null)
(define theta-of-unit null)
(define l null)
(define heuristic-list (list 2 1.5 1.5 5 3))
(define peg-removed initial-board)
(define current-board initial-board)
(define prev-config initial-board)
(define next-list '())
(define valid-slots (filter (lambda (x) (part-board? x board)) (cprod (range n) (range n))))
(define player-posns-list (make-vector n-players '()))

;; Vector
(define vboard (make-2d-vector size size -1))
(map (lambda (x) (2d-vector-set! vboard (car x) (cdr x) 0)) valid-slots)


(define (fill-vector-posns i board)
  (if (= i 0) vboard
      (begin (map (lambda (x) (2d-vector-set! vboard (car x) (cdr x) i))
                              (filter (lambda (x) (player-posns? i (car x) (cdr x) board)) (cprod (range n) (range n))))
             (fill-vector-posns (- i 1) board))))


(fill-vector-posns n-players board)
(define current-pegs (vector-copy player-posns-list))

(define (update-posns-list initial final current-player)
  (let* ([orig-pegs (vector-ref current-pegs (- current-player 1))])
    (vector-set! current-pegs (- current-player 1) (cons final (remove initial orig-pegs)))))

  (define (fill-posns-list i board)
    (if (= i 0) (void)
        (begin
          (vector-set! player-posns-list (- i 1) (filter (lambda (x) (player-posns? i (car x) (cdr x) board)) (cprod (range n) (range n))))
          (fill-posns-list (- i 1) board))))

  (fill-posns-list n-players board)

(define (peg-for-player player board)
  (circle slot-radius "solid" (list-ref player-colors (- player 1))))

(define (next-pegs-player player board)
  (circle slot-radius "solid" (list-ref player-next-colors (- player 1))))

(define (index->coords x y)
    (if (= 0 (modulo x 2)) (cons (* (/ (image-width unit1) 2) (+ 1 (* 2 y))) (* (/ (image-height unit1) 2) (+ 1 (* 2 (quotient x 2)))))
        (cons (* (/ (image-width unit1) 2) (+ 2 (* 2 y))) (* (/ (image-height unit1) 2) (+ 2 (* 2 (quotient x 2)))))))

(define (ind->posns cords)
  (let* ([ans (index->coords (car cords) (cdr cords))])
    (make-posn (car ans) (cdr ans))))

(define (place-initial-pegs i in-board)
  (if (= i 0) in-board
      (place-initial-pegs (- i 1)
                          (place-images (make-list pegs-per-player (peg-for-player i board))
                                        (map ind->posns (vector-ref player-posns-list (- i 1))) in-board))))


(define (create-board board)
  
  (set! theta-of-unit (if (or (= board 2) (= board 3)) 90 60))
                          
  (set! unit1
    (cond
      [(= board 1) (overlay (circle slot-radius "solid" "gray") (rhombus unit-size theta-of-unit "solid" "white"))]
      [(= board 2) (rhombus (- unit-size 0.4) theta-of-unit "solid" "brown")]
      [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size theta-of-unit "solid" "white"))]))

  (set! unit2
    (cond
      [(= board 1) (overlay (circle slot-radius "solid" "gray") (rhombus unit-size theta-of-unit "solid" "white"))]
      [(= board 2) (rhombus (- unit-size 0.4) theta-of-unit "solid" "LightGoldenrodYellow")]
      [(= board 3) (overlay (circle (- (/ unit-size 2) 1) "outline" "black") (rhombus unit-size theta-of-unit "solid" "white"))]))

  (set! l (make-list n unit1))

  (set! posns (map (lambda (x) (make-posn (* (image-width unit1) (+ x (/ 1 2))) (/ (image-height unit1) 2))) (range n)))

  (define (place-initial-pegs i in-board)
    (if (= i 0) in-board
        (place-initial-pegs (- i 1)
                            (place-images (make-list pegs-per-player (peg-for-player i board))
                                          (map ind->posns (filter (lambda (x) (player-posns? i (car x) (cdr x) board)) (cprod (range n) (range n)))) in-board))))

  (define (get-single-row unit)
    (place-images (make-list n unit) posns (rectangle x-size (image-height unit1) "solid" "transparent")))

  (define row-couple (underlay/xy (get-single-row unit1) (/ (image-width unit1) 2) (/ (image-height unit1) 2) (get-single-row unit2)))

  (define size-x (image-width row-couple))
  (define size-y (image-height row-couple))

  (set! posns_board (map (lambda (x) (make-posn (/ size-x 2) (+ (/ size-y 2) (* x (image-height unit1))))) (range n)))

  (set! full-board (place-images (make-list n row-couple) posns_board (rectangle x-size y-size "solid" "transparent")))

  (set! player-posns-list (make-vector n-players '()))


  (fill-posns-list n-players board)


  (set! empty-slots (filter (lambda (x) (not-in-board? x board)) (cprod (range n) (range n))))

  (set! empty-board (place-images (make-list (length empty-slots) peg) (map ind->posns empty-slots) full-board))

  (set! initial-board (place-initial-pegs n-players empty-board))

  (set! peg-removed initial-board)

  (set! current-pegs (vector-copy player-posns-list))

  
  (set! current-board initial-board)

  (set! prev-config initial-board)

  (set! next-list '())

  (set! valid-slots (filter (lambda (x) (part-board? x board)) (cprod (range n) (range n))))

  (set! vboard (make-2d-vector size size -1))
  (map (lambda (x) (2d-vector-set! vboard (car x) (cdr x) 0)) valid-slots)

  (fill-vector-posns n-players board))

(define board2
  (begin
    (set! board 2)
    (create-board board)
    initial-board))

(define board1
  (begin
    (set! board 1)
    (create-board board)
    initial-board))
  
; Player1: Computer (AI)
; Player2: User
; Player2 starts the game
; State5: Player2's turn
; State6: If player2 has clicked : button down on his peg -> state7 display nextpossible moves
; State7: if drag, move peg with mouse: if button-up: if valid next possible slot put it there >  state8, else place it in orig pos > state5
; State8: Game Over-> state15, else state10
; State10: AI's Turn : get best move from minimax, with time move/animate from current pos to 

(define (remove-peg curr-board i j)
  (let* ([coords (index->coords i j)])
    (place-image (if (even? i) unit1 unit2) (car coords) (cdr coords) curr-board)))

(define (inside-a-slot? x y i j board)
  (let* ([center (index->coords i j)]
         [xc (car center)]
         [yc (cdr center)])
    (cond [(or (= board 1) (= board 3)) (< (+ (expt (- x xc) 2) (expt (- y yc) 2)) (* slot-radius slot-radius))]
          [(= board 2) (let* ([cs (cos (/ theta-of-unit 2))]
                              [cot (/ 1 (tan (/ theta-of-unit 2)))])
                         (and (< (- y yc (* unit-size cs)) (- (* cot (- x xc))))
                              (< (- y yc (* unit-size cs)) (* cot (- x xc)))
                              (> (+ (- y yc) (* unit-size cs)) (- (* cot (- x xc))))
                              (> (+ (- y yc) (* unit-size cs)) (* cot (- x xc)))))])))

(define (place-peg image player i j ind)
  (let* ([coords (if ind (index->coords i j) (cons i j))])
    (place-image (peg-for-player player board) (car coords) (cdr coords) image)))



(define (get-index-of-clicked x y b)
  (filter (lambda (z) (inside-a-slot? x y (car z) (cdr z) b)) valid-slots))
  


(define select-mode-scene (place-images (list
                                         (overlay/align "center" "center" (text "Player1 vs Player2" 20 "indigo") (rectangle 200 50 "outline" "black"))
                                         (overlay/align "center" "center" (text "Player vs Computer" 20 "indigo") (rectangle 200 50 "outline" "black"))
                                         (overlay/align "center" "center" (text "AI vs AI Simulation" 20 "indigo") (rectangle 450 50 "outline" "black")))
                                        (list (make-posn 250 300) (make-posn 550 300) (make-posn 400 375))
                                        (empty-scene 800 600)))

(define end-scene (place-images (list
                                         (overlay/align "center" "center" (text "Game Ends" 20 "indigo") (rectangle 200 50 "outline" "white"))
                                         (overlay/align "center" "center" (text "Play Again" 20 "indigo") (rectangle 150 50 "outline" "black"))
                                         (overlay/align "center" "center" (text "Exit" 20 "indigo") (rectangle 150 50 "outline" "black")))
                (list (make-posn 400 150) (make-posn 250 350) (make-posn 550 350))
                (empty-scene 800 600)))

(define options-scene
  (place-images (list
                  (overlay/align "center" "center" (text "CHINESE CHECKERS" 50 "Indigo") (rectangle 200 50 "outline" "white"))
                  (overlay/align "center" "center" (text "SELECT BOARD SHAPE" 20 "Indigo") (rectangle 300 50 "outline" "black"))
                  (overlay/align "center" "center" (text "SELECT DIFFICULT LEVEL" 20 "Indigo") (rectangle 300 50 "outline" "black"))
                  (overlay/align "center" "center" (text "START !" 20 "Indigo") (rectangle 200 50 "outline" "black")))
                (list (make-posn 375 100) (make-posn 375 200) (make-posn 375 300) (make-posn 375 400))
                                           (empty-scene 800 600)))

(define board-select-scene
  (place-images (list
                  (scale 0.5 board1)
                  (scale 0.5 board2))
                  ;(overlay/align "center" "center" (text "BACK" 20 "Indigo") (rectangle 200 50 "outline" "black")))
                (list (make-posn 200 250) (make-posn 600 275)) ;(make-posn 400 500))
                                           (empty-scene 800 600)))

(define difficulty-select-scene
  (place-images (list
                  (overlay/align "center" "center" (text "LEVEL 1" 20 "Indigo") (rectangle 200 50 "outline" "black"))
                  (overlay/align "center" "center" (text "LEVEL 2" 20 "Indigo") (rectangle 200 50 "outline" "black"))
                  (overlay/align "center" "center" (text "LEVEL 3" 20 "Indigo") (rectangle 200 50 "outline" "black")))
                  ;(overlay/align "center" "center" (text "BACK" 20 "Indigo") (rectangle 200 50 "outline" "black")))
                (list (make-posn 375 200) (make-posn 375 300) (make-posn 375 400)) ;(make-posn 375 500))
                                           (empty-scene 800 600)))

(define initial (cons 0 0))

(define move-path '())

(struct display-state (n time) #:transparent)
(define current-player 2)

(define (create-scene state)
  (cond
    [(= (display-state-n state) 0) select-mode-scene]
    [(= (display-state-n state) 2) options-scene]
    [(= (display-state-n state) 3) board-select-scene]
    [(= (display-state-n state) 4) difficulty-select-scene]
    [(= (display-state-n state) 11)
     (place-image (overlay/align "center" "center"
                                 (cond [(= mode 2) (if (= current-player 1) (text "Computer wins. You lose! :(" 20 "red")
                                                                               (text "You win :)" 20 "green"))]
                                       [(= mode 1) (if (= current-player 2) (text "Green wins" 20 "green")
                                                                               (text "Red wins" 20 "red"))]
                                       [(= mode 3) (if (= current-player 2) (text "AI-1 wins" 20 "green")
                                                                               (text "AI-2 wins" 20 "red"))]
                                       [else (text "!" 20 "red")])
                                 (rectangle 200 50 "solid" "white")) 400 250 end-scene)] 
    [else (place-image (overlay/align "center" "center" (cond [(= mode 2) (if (= current-player 1) (text "Computer's turn. Please wait!" 20 "red")
                                                                               (text "Your turn" 20 "green"))]
                                                              [(= mode 1) (if (= current-player 2) (text "Green's turn" 20 "green")
                                                                               (text "Red's turn" 20 "red"))]
                                                              [(= mode 3) (if (= current-player 2) (text "AI-1 move" 20 "green")
                                                                               (text "AI-2 move" 20 "red"))])
                                                   (rectangle 100 40 "outline" "white")) 350 60 current-board)]))

(define (handle-mouse-events state x y event)
  (cond [(mouse=? event "button-down") (handle-button-down state x y)]
        [else state]))

;;;;;;;;;;;;;;; AI move functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-random-ai-move current-player)
  (let* [(next-moves (append* (map (lambda (x) (map (lambda (y) (list x y)) (next-move x current-pegs 1 board)))
                                   (current-player-pegs vboard current-player board))))]
    (list-ref next-moves (random (length next-moves)))))

(define (get-minimax-ai-move current-player depth l)
  ; The list of parameters to the minimax is in the following order (wvertical whop wbackpiece wedge whorizontal)
  (let* ([mv (minimax #t current-player current-player depth depth  board -inf.0 +inf.0 null l player-posns-list current-pegs)]
         [path (assoc (cadr mv) (next-move (car mv) current-pegs current-player board))])
    (list (car mv) path)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (handle-button-down state x y)
  (cond
    [(= (display-state-n state) 11)
        (cond
          [(and (>= x 175) (<= x 325) (>= y 325) (<= y 375))
          (begin
            (create-board 1)
            (display-state 0 0))]
          [(and (>= x 475) (<= x 625) (>= y 325) (<= y 375))
           (display-state 12 0)]
          [else state])]
    
    [(= (display-state-n state) 0)
     (cond [(and (>= x 150) (<= x 350) (>= y 275) (<= y 325))
            (begin (set! mode 1) (set! current-player 2) (display-state 5 (display-state-time state)))]
           [(and (>= x 450) (<= x 650) (>= y 275) (<= y 325))
            (begin (set! mode 2) (set! current-player 2) (display-state 5 (display-state-time state)))]
           [(and (>= x 175) (<= x 625) (>= y 350) (<= y 400))
            (begin (set! mode 3) (set! current-player 2) (display-state 8 (display-state-time state)))]
           [else state])]

    [(= (display-state-n state) 2)
     (cond [(and (>= x 225) (<= x 525) (>= y 175) (<= y 225)) (display-state 3 (display-state-time state))]
           [(and (>= x 225) (<= x 525) (>= y 275) (<= y 325)) (display-state 4 (display-state-time state))]
           [(and (>= x 275) (<= x 475) (>= y 375) (<= y 425)) (display-state 0 (display-state-time state))]
           [else state])]

    [(= (display-state-n state) 3)
     (cond [(and (>= x 50) (<= x 350) (>= y 50) (<= y 400))
                 (begin (set! board 1)
                        (create-board board)
                        (display-state 2 (display-state-time state)))]
           [(and (>= x 450) (<= x 800) (>= y 75) (<= y 475))
                 (begin (set! board 2)
                        (create-board board)
                        (display-state 2 (display-state-time state)))]
           ;[(and (>= x 300) (<= x 500) (>= y 475) (<= y 525))
           ;      (display-state 2 (display-state-time state))]
           [else state])]

    [(= (display-state-n state) 4)
       (cond [(and (>= x 275) (<= x 475) (>= y 175) (<= y 225))
                (begin
                (set! heuristic-list (list 2 1.5 1.5 5 3))
                (display-state 2 (display-state-time state)))]
             [(and (>= x 275) (<= x 475) (>= y 275) (<= y 325))
                (begin
                (set! heuristic-list (list 2 1.5 1.5 5 3))
                (display-state 2 (display-state-time state)))]
             [(and (>= x 275) (<= x 475) (>= y 375) (<= y 425))
                (begin
                (set! heuristic-list (list 2 1.5 1.5 5 3))
                (display-state 2 (display-state-time state)))]
             ;[(and (>= x 275) (<= x 475) (>= y 475) (<= y 525))
             ;  (begin
             ;  (set! heuristic-list (list 2 1.5 1.5 5 3))
             ;  (display-state 2 (display-state-time state)))]
             [else state])]
    
    [(= (display-state-n state) 5)
          (let* ([ind (get-index-of-clicked x y board)])
            (if (and (not (null? ind)) (= current-player (2d-vector-ref vboard (caar ind) (cdar ind))))
                (begin (set! prev-config current-board)
                       (let* ([next (next-move (car ind) current-pegs current-player board)])
                         (set! initial (car ind))
                         (set! peg-removed (remove-peg current-board (caar ind) (cdar ind)))
                         (set! current-board (place-images (make-list (length next) (next-pegs-player current-player board)) (map (lambda (x) (ind->posns(car x))) next) current-board))
                         (set! next-list next)) (display-state 6 (display-state-time state))) (display-state 5 (display-state-time state))))]
        [(= (display-state-n state) 6) 
         (let* ([ind (get-index-of-clicked x y board)])
           
           (cond [(and (not (null? ind)) (member (car ind) (map car next-list)))
                  (begin
                    (2d-vector-set! vboard (car initial) (cdr initial) 0)
                    (2d-vector-set! vboard (caar ind) (cdar ind) current-player)
                    (update-posns-list initial (car ind) current-player)
             
                    (set! move-path (reverse (assoc (car ind) next-list)))
                    (display-state 7 (display-state-time state)))]
                 
                 [(and (not (null? ind)) (= (2d-vector-ref vboard (caar ind) (cdar ind)) current-player)) (begin (set! current-board prev-config)
                                                                                                                 (handle-button-down (display-state 5 (display-state-time state)) x y))]
                 [else state]))]
       [else state]))


(define (handle-tick state)
  
  (cond
    [(or (= (display-state-n state) 7) (= (display-state-n state) 9))
     
     (cond [(null? move-path)
           (if (or (is-endgame? current-player current-pegs player-posns-list)
                   (is-endgame? (get-opposite-player current-player) current-pegs player-posns-list))
               (display-state 11 (display-state-time state))
               (begin
                 (set! current-player (if (= current-player n-players) 1 (+ 1 current-player)))
                 (display-state (cond [(= mode 2) (if (= (display-state-n state) 7) 8 5)]
                                      [(= mode 1) 5]
                                      [(= mode 3) 8]) (display-state-time state))))]
           [else 
            (begin
                   (set! current-board (place-peg peg-removed current-player (caar move-path) (cdar move-path) vboard))
                   (set! peg-removed (remove-peg current-board (caar move-path) (cdar move-path)))
                   (set! move-path (cdr move-path))
                   (display-state (display-state-n state) (add1 (display-state-time state))))])]
    [(= (display-state-n state) 8) (let* ([ind (if (and (= mode 3) (= current-player 2))
                                 ; The list of parameters to the minimax is in the following order (wvertical whop wbackpiece wedge whorizontal)

                                                                  (get-minimax-ai-move current-player 2 (list 2 1.5 1.5 5 3))
                                                                  (get-minimax-ai-move current-player 4 (list 2 1.5 1.5 5 3)))])
                        (begin
                        (set! peg-removed (remove-peg current-board (caar ind) (cdar ind)))
                        (2d-vector-set! vboard (caar ind) (cdar ind) 0)
                        (2d-vector-set! vboard (caaadr ind) (cdaadr ind) current-player)
                        (update-posns-list (car ind) (caadr ind) current-player)
                        (set! move-path (reverse (cadr ind)))
                        (display-state 9 (add1 (display-state-time state)))))]
    
    [else (display-state (display-state-n state) (add1 (display-state-time state)))]))

(big-bang (display-state 2 0)
          (stop-when (lambda (state) (= (display-state-n state) 12)))
          (on-tick handle-tick)
          (close-on-stop #t)
          (on-mouse handle-mouse-events) 
          (to-draw create-scene))
