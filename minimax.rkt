#lang racket
(provide (all-defined-out))
(require "board_utils.rkt")
(require racket/vector)

(define size 30)

(define (make-2d-vector r c initial)
  (build-vector r (lambda (x) (make-vector c initial))))

(define (2d-vector-ref vec r c)
  (vector-ref (vector-ref vec r) c))

(define (2d-vector-set! vec r c val)
  (let ((v (vector-ref vec r)))
    (begin
      (vector-set! v c val))))

(define (is-endgame? current-player orig-pegs goal)
  (let* ((filtered-posns-1 (equal? (list->set (vector-ref orig-pegs 0)) (list->set (vector-ref goal 1))))
         (filtered-posns-2 (equal? (list->set (vector-ref orig-pegs 1)) (list->set (vector-ref goal 0)))))
        (cond [(= 1 current-player) filtered-posns-1]
              [(= 2 current-player) filtered-posns-2])))

(define (occupied-slot? i j orig-pegs)
  (member (cons i j) (append* (vector->list orig-pegs))))

(define (empty-slot? i j orig-pegs board)
  (and (not (member (cons i j) (append* (vector->list orig-pegs)))) (part-of-board? i j board)))

(define (get-direction-functions i)
  (if (even? i) (list (cons sub1 sub1)
                      (cons sub1 identity)
                      (cons add1 identity)
                      (cons add1 sub1)
                      (cons identity sub1)
                      (cons identity add1))
                (list (cons sub1 identity)
                      (cons sub1 add1)
                      (cons add1 add1)
                      (cons add1 identity)
                      (cons identity sub1)
                      (cons identity add1))))


; Returns the list of neighbouring slots starting with the one on top-left in cw order
; Doesn't check if the slot is out-of-board
(define (next-neighbour i j)
  (map (lambda (x) (cons ((car x) i) ((cdr x) j))) (get-direction-functions i)))  

; Returns the list of slots one hop away starting with the one on top-left in cw order
; Doesn't check if the slot is out-of-board
(define (second-nearest-neighbour i j)
  (let* ([f1 (get-direction-functions i)]
         [f2 (get-direction-functions (+ i 1))])
  (map (lambda (x) (cons ((cadr x) ((caar x) i)) ((cddr x) ((cdar x) j)))) (zip f1 f2))))

(define (zip l1 l2)
  (if (null? l2) null (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2)))))

(define (possible-hops i j orig-pegs board)
  (let* ([next-zip-hop (zip (next-neighbour i j) (second-nearest-neighbour i j))])
    (map (lambda (x) (cdr x)) (filter (lambda (x) (and (>= (cadr x) 0) (>= (cddr x) 0)
                                                   (< (cadr x) size) (< (cddr x) size)
                                                   (occupied-slot? (caar x) (cdar x) orig-pegs)
                                                   (empty-slot? (cadr x) (cddr x) orig-pegs board))) next-zip-hop))))
                                                   
(define (next-move pos orig-pegs current-player board)
  (filter (lambda (x) (and (>= (caar x) 0) (>= (cdar x) 0) (< (caar x) size) (< (cdar x) size)
                           (empty-slot? (caar x) (cdar x) orig-pegs board)))
          (append (map list (next-neighbour (car pos) (cdr pos))) (walk-through-hop orig-pegs pos (list pos) '() board))))

(define (walk-through-hop orig-pegs pos l path board)
  (let* ([single-hop (filter (lambda (x) (not (member x l))) (possible-hops (car pos) (cdr pos) orig-pegs board))])
    (if (null? single-hop) '() (remove-duplicates (append (map (lambda (x) (cons x path)) single-hop) (append* (map (lambda (x) (walk-through-hop orig-pegs x (append single-hop l) (cons x path) board)) single-hop))))))) 

 (define (get-opposite-player x)
  (if (= x 1) 2 1))

;; Evaluate Board Function
  
(define (evaluate-board orig-pegs current-player board-type move parameters goal)

  (define (vertical-distance row current-player)
    (cond [(= 1 current-player) (abs (- row 3))]
          [(= 2 current-player) (abs (- row 21))]))

 (define current-endgame (is-endgame? current-player orig-pegs goal))
 (define other-endgame (is-endgame? (get-opposite-player current-player) orig-pegs goal))

 (define (game-progress line current-player)
   (length (filter (lambda (x) (> (vertical-distance (car x) current-player) line)) (vector-ref orig-pegs (- current-player 1)))))

   (define g (game-progress 12 current-player))

  (define whorizontal (list-ref parameters 0))
  ;(define wvertical (if (>= g 9) (* 100 (list-ref parameters 0)) (list-ref parameters 0)))
   (define whop (list-ref parameters 1))
   (define wbackmove (list-ref parameters 2))
   (define iedge (list-ref parameters 3))
   ;(define whorizontal (if (= g 10) 0 (list-ref parameters 4)))
   (define whorizontal (list-ref parameters 4))
   (define move-score1 (- (vertical-distance (caadr move) current-player) (vertical-distance (caar move) current-player)))
   (define move-score2 (- 18 (vertical-distance (caar move) current-player)))

 (define (score-evaluater row column current-player board-type)
   
   (define (horizontal-distance)
     (let* ([centre (if (even? row) 10.5 11)]
            [score (- centre (abs (- centre column)))])
       score))
   
   (define (is-edge? board-type)
     (cond [(= 2 current-player) (and  (> row 3)
                                       (< 8 row)
                                       (if (even? row) (or (= column (- 12 (/ row 2)))
                                                           (= column (+ 8 (/ row 2))))
                                           (or (= row (- 23 (* 2 column)))
                                               (= row (- 15 (* 2 column))))))]
           [(= 1 current-player) (and (< row 21) (> row 16)
                                      (if (even? row) (or (= column (/ row 2))
                                                          (= (* 2 column) (- 40 row)))
                                          (or (= column (quotient row 2))
                                              (= (* 2 column) (- 39 row)))))]))

   (let ([n-score (+ (* wvertical (vertical-distance row current-player)) (* whorizontal (horizontal-distance)))])
     (cond ;[(player-posns? current-player row column board-type) (/ -22 (vertical-distance row current-player))]
           [(player-posns? (get-opposite-player current-player) row column board-type)
            (if (and (is-edge? board-type)) (+ n-score iedge) n-score)]
           [else n-score])))


  
  (define opposite-player (get-opposite-player current-player))

  (define Total-self
    (foldl (lambda (x y) (+ y (score-evaluater (car x) (cdr x) current-player board-type))) 0 (vector-ref orig-pegs (- current-player 1))))
  
  (define Total-opponent
    (foldl (lambda (x y) (+ y (score-evaluater (car x) (cdr x) opposite-player board-type))) 0 (vector-ref orig-pegs (- opposite-player 1))))

;  (display "Current Player : ") (display current-player) (newline)
;  (display "Back Piece Score : ") (display (* wbackmove move-score2)) (newline)
;  (display "Hop Score : ") (display (* whop move-score1)) (newline)
;  (display "Total Self : ") (display Total-self) (newline)
;  (display "Total Opponent : ") (display Total-opponent) (newline)
 
  (cond [current-endgame 1000000]
        [other-endgame -1000000]
        [else (- Total-self Total-opponent)]))
        ;[else (+ (* wbackmove move-score2) (* whop move-score1) (- Total-self Total-opponent))]))

(define (move-filter current-player maximum-back initial-pos final-pos-list)
  (define (vertical-distance row)
    (cond [(= 1 current-player) (abs (- row 3))]
          [(= 2 current-player) (abs (- row 21))]))
  
  (let* ([initial-vert (car initial-pos)]
         [final-vert (caar final-pos-list)])
    (< (- maximum-back) (- (vertical-distance final-vert)
                           (vertical-distance initial-vert)))))
  
;; Minimax Function

(define (minimax is-maximising-player? current-player root-player depth max-depth board-type alpha beta move parameters goal orig-pegs)

  (define best-val
    (cond [is-maximising-player? -inf.0]
          [else +inf.0]))

  (define (vertical-distance row current-player)
    (cond [(= 1 current-player) (abs (- row 3))]
          [(= 2 current-player) (abs (- row 21))]))

  (define whop (list-ref parameters 1))
  (define wbackmove (list-ref parameters 2))

  (define (make-move board pos1 pos2)
    (let* ([i1 (car pos1)]
           [j1 (cdr pos1)]
           [i2 (car pos2)]
           [j2 (cdr pos2)]
           [peg (2d-vector-ref board i1 j1)]
           [board1 (for/vector ((i size)) (vector-copy (vector-ref board i)))])
      (2d-vector-set! board1 i1 j1 0)
      (2d-vector-set! board1 i2 j2 peg)
      board1))
  
  (define (move-pegs current-pegs initial final)
    (let* ([orig-pegs (vector-ref current-pegs (- current-player 1))]
           [v-copy (vector-copy current-pegs)])
      (begin (vector-set! v-copy (- current-player 1) (cons final (remove initial orig-pegs)))
             v-copy)))

  
  (define (compare val1 val2)
   (cond [is-maximising-player? (> (caddr val1) (caddr val2))]
         [else (< (caddr val1) (caddr val2))]))
                         
  (define (minimax-helper1 orig-pegs pos next-move-list init alpha beta)
    (cond [(null? next-move-list) init]
          [else (let* ([next-pos (caar next-move-list)]
                       [new-posns (move-pegs orig-pegs pos next-pos)]
                       [opposite-player (get-opposite-player current-player)]
                       [top-move (if (= depth max-depth) (list pos next-pos) move)]
                       [original-val (minimax (not is-maximising-player?) opposite-player root-player (- depth 1) max-depth board-type alpha beta top-move parameters goal new-posns)]
                       [move-score1 (- (vertical-distance (car next-pos) current-player) (vertical-distance (car pos) current-player))]
                       [move-score2 (- 18 (vertical-distance (car pos) current-player))]
                       [val
                        (list (car original-val) (cadr original-val) (+ (* wbackmove move-score2) (* whop move-score1) (caddr original-val)))]
                        
                       [optVal (if (compare val init) val init)]
                       [alpha-new (if is-maximising-player? (max alpha (caddr optVal)) alpha)]
                       [beta-new (if (not is-maximising-player?) (min beta (caddr optVal)) beta)])
                (cond [(<= beta-new alpha-new) (if (compare val init) (list pos next-pos (caddr optVal)) init)]
                      [(compare optVal init) (minimax-helper1 orig-pegs pos (cdr next-move-list) (list pos next-pos (caddr optVal)) alpha-new beta-new)]
                      [else (minimax-helper1 orig-pegs pos (cdr next-move-list) init alpha-new beta-new)]))]))

  (define (minimax-helper2 orig-pegs current-positions init alpha beta)
    (cond [(null? current-positions) init]
          [else (let* ([pos (car current-positions)]
                       [original-next-move-list (next-move pos orig-pegs current-player board-type)]
                       [filtered-next-move-list (filter (lambda(x) (move-filter current-player 4 pos x)) original-next-move-list)]
                       [next-move-list (if (null? filtered-next-move-list) original-next-move-list filtered-next-move-list)]
                       [val (minimax-helper1 orig-pegs pos next-move-list init alpha beta)]
                       [optVal (if (compare val init) val init)]
                       [alpha-new (if is-maximising-player? (max alpha (caddr optVal)) alpha)]
                       [beta-new (if (not is-maximising-player?) (min beta  (caddr optVal)) beta)])
                 (if (<= beta-new alpha-new) optVal
                      (minimax-helper2 orig-pegs (cdr current-positions) optVal alpha-new beta-new))
                )]))

  (let* ([current-positions (vector-ref orig-pegs (- current-player 1))]
         [init (list (cons 0 0) (cons 0 0) best-val)])
    (if (or (= depth 0) (is-endgame? 1 orig-pegs goal) (is-endgame? 2 orig-pegs goal))
               (list (cons 0 0) (cons 0 0) (evaluate-board orig-pegs root-player board-type move parameters goal))
               (minimax-helper2 orig-pegs current-positions init alpha beta))))

;Returns a list of cons containing the positions of pegs of current-player
(define (current-player-pegs board current-player board-type) 
    (define (current-player-helper board row)
      (define required-row (vector-ref board row))
      (define (helper vec i acc)
        (cond [(= i size) acc]
              [else (if (= (vector-ref vec i) current-player)
                               (helper vec (+ i 1) (cons (cons row i) acc))
                               (helper vec (+ i 1) acc))]))
      (helper required-row 0 null))
    (foldl (lambda(x y) (append (current-player-helper board x) y)) null (build-list size (lambda(x) x))))