#lang racket
(provide (all-defined-out))

(define (part-of-board? i j board)
  (cond
  [(or (= board 1) (= board 3)) (or (and (< i 17) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))
      (and (< i 21) (> i 7) (if (= 0 (modulo i 2)) (and (>= j (/ i 2)) (<= (* 2 j) (- 40 i)))
                                (and (>= j (quotient i 2)) (<= (* 2 j) (- 39 i))))))]
  [(= board 2) (or (and (< i 12) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))
                   (and (> i 11) (< i 19) (if (= 0 (modulo i 2)) (and (>= (* 2 j) (+ i 2)) (<= (* 2 j) (- 38 i)))
                                              (and (>= (* 2 j) (+ i 1)) (<= (* 2 j) (- 37 i))))))]))

(define (player-posns? player i j board)
  (cond [(or (= board 1) (= board 3))
         (cond [(= player 1) (and (< i 8) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))]
               [(= player 2)  (and (< i 21) (> i 16) (if (= 0 (modulo i 2)) (and (>= j (/ i 2)) (<= (* 2 j) (- 40 i)))
                                (and (>= j (quotient i 2)) (<= (* 2 j) (- 39 i)))))])]
        [(= board 2)
         (cond [(= player 1) (and (< i 8) (> i 3) (if (= 0 (modulo i 2)) (and (>= j (- 12 (/ i 2))) (<= j (+ 8 (/ i 2))))
                                (and (>= i (- 23 (* 2 j))) (>= i (- (* 2 j) 15)))))]
               [(= player 2) (and (> i 14) (< i 19) (if (= 0 (modulo i 2)) (and (>= (* 2 j) (+ i 2)) (<= (* 2 j) (- 38 i)))
                                              (and (>= (* 2 j) (+ i 1)) (<= (* 2 j) (- 37 i)))))])]))

(define (not-in-board? coord board)
  (not (part-of-board? (car coord) (cdr coord) board)))

(define (part-board? coord board)
  (part-of-board? (car coord) (cdr coord) board))

(define (cprod l1 l2)
  (append* (map (lambda (y) (map (lambda (x) (cons y x)) l2)) l1)))
