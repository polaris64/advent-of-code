#lang racket

(require file/convertible)
(require pict)

(define test-input-1 "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<
")

(define test-input-2 "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (extract-cells-by-type cells type)
  (filter (λ (cell) (eq? (cdr cell) type)) cells))

(define (parse-input inp-str)
  (let ((grid (takef (string-split inp-str "\n") (λ (line) (not (regexp-match #rx"^$" line)))))
        (moves (cdr (dropf (string-split inp-str "\n") (λ (line) (not (regexp-match #rx"^$" line)))))))
    (let ((grid-cells (apply append (map (λ (y row)
                                           (map (λ (x cell)
                                                  (cons (cons x y) cell))
                                                (range (string-length row)) (string->list row)))
                                         (range (length grid)) grid))))
      (values
       (list->vector (map (λ (row)
                            (list->vector
                             (map (λ (cell)
                                    (if (eq? cell #\@) #\. cell))
                                  (string->list row)))
                            )
                          grid))
       (map car (extract-cells-by-type grid-cells #\#))
       (map car (extract-cells-by-type grid-cells #\@))
       (map car (extract-cells-by-type grid-cells #\O))
       (apply append
              (map (λ (line) (string->list line)) moves))))))

(define (make-vec x y) (cons x y))

(define (vec-x v) (car v))

(define (vec-y v) (cdr v))

(define (is-box? cell) (eq? cell #\O))

(define (is-empty? cell) (eq? cell #\.))

(define (is-wall? cell) (eq? cell #\#))

(define (get-cell-at grid pos)
  (if (or (< (vec-x pos) 0)
          (< (vec-y pos) 0)
          (>= (vec-x pos) (vector-length (vector-ref grid 0)))
          (>= (vec-y pos) (vector-length grid)))
      #f
      (vector-ref (vector-ref grid
                              (vec-y pos))
                  (vec-x pos))))

(define (get-span-start grid pos dx dy)
  (let ((new-pos (make-vec (+ (vec-x pos) dx)
                           (+ (vec-y pos) dy))))
    (if (not (is-box? (get-cell-at grid new-pos)))
        pos
        (get-span-start grid new-pos dx dy))))

(define (push-span! grid start-pos end-pos dx dy)
  (let ((sx (if (< (vec-x start-pos) (vec-x end-pos))
                (vec-x start-pos)
                (vec-x end-pos)))
        (ex (+ 1 (if (< (vec-x start-pos) (vec-x end-pos))
                     (vec-x end-pos)
                     (vec-x start-pos))))
        (sy (if (< (vec-y start-pos) (vec-y end-pos))
                (vec-y start-pos)
                (vec-y end-pos)))
        (ey (+ 1 (if (< (vec-y start-pos) (vec-y end-pos))
                     (vec-y end-pos)
                     (vec-y start-pos)))))
    ;; Clear span
    (for-each (λ (y)
                (for-each (λ (x)
                            (let ((row (vector-ref grid y)))
                              (vector-set! row x #\.)
                              (vector-set! grid y row)))
                          (range sx ex)))
              (range sy ey))

    ;; Create new span
    (for-each (λ (y)
                (for-each (λ (x)
                            (let ((row (vector-ref grid y)))
                              (vector-set! row x #\O)
                              (vector-set! grid y row)))
                          (if (< dx 0)
                              (range sx (+ ex dx))
                              (range (+ sx dx) ex))))
              (if (< dy 0)
                  (range sy (+ ey dy))
                  (range (+ sy dy) ey)))

    grid))

(define (perform-step grid robot-pos move)
  (define (push-boxes curr-pos new-pos)
    (when (is-box? (get-cell-at grid new-pos))
      (let ((dx (- (vec-x new-pos) (vec-x curr-pos)))
            (dy (- (vec-y new-pos) (vec-y curr-pos))))

        ;; Find span of boxes in direction of travel
        (let ((span-start-pos (get-span-start grid new-pos dx dy)))
          (let ((span-new-start (make-vec (+ (vec-x span-start-pos) dx)
                                          (+ (vec-y span-start-pos) dy))))

            ;; Check if span can be pushed
            (if (is-empty? (get-cell-at grid span-new-start))

                (begin
                  ;; Push span
                  (push-span! grid span-new-start new-pos dx dy)
                  new-pos)
                curr-pos))))))

  (define (move-robot dx dy)
    (let* ((new-pos (make-vec (+ (vec-x robot-pos) dx)
                              (+ (vec-y robot-pos) dy)))
           (target-cell (get-cell-at grid new-pos)))

      (if (and target-cell (not (is-wall? target-cell)))
          (begin 
            (if (is-box? target-cell)
                (push-boxes robot-pos new-pos) 
                new-pos))
          robot-pos)))

  (cond ((eq? move #\^) (move-robot 0 -1))
        ((eq? move #\v) (move-robot 0 1))
        ((eq? move #\<) (move-robot -1 0))
        ((eq? move #\>) (move-robot 1 0))
        (else (error "perform-step: unknown move:" move))))

(define (run-moves inp [visualise? #f])
  (call-with-values
   (λ () (parse-input inp))
   (λ (grid walls robots boxes moves)
     (printf "Walls: ~a, robots: ~a, boxes: ~a, moves: ~a\n"
             (length walls)
             robots
             (length boxes)
             (length moves))
     (let ((final-pos (foldl (λ (move move-num robot-pos)
                               ;; (printf "Robot pos: ~a, next move: ~a\n" robot-pos move)
                               (let ((res (perform-step grid robot-pos move)))
                                 (when visualise?
                                   (call-with-output-file (format "/tmp/grid-~a.png"
                                                                  (~a move-num
                                                                      #:min-width 5
                                                                      #:align 'right
                                                                      #:left-pad-string "0"))
                                     (λ (port)
                                       (write-bytes
                                        (convert
                                         (visualise-grid grid res)
                                         'png-bytes)
                                        port))))
                                 res))
                             (car robots)
                             moves
                             (range (length moves)))))
       (values final-pos grid)))))

(define (visualise-grid grid robot-pos)
  (let ((scale 5))
    (apply vc-append
           (map (λ (y row)
                  (apply ht-append
                         (map (λ (x cell)
                                (colorize (filled-rectangle scale scale)
                                          (cond ((equal? (make-vec x y) robot-pos) "green")
                                                ((is-box? cell) "purple")
                                                ((is-wall? cell) "black")
                                                ((is-empty? cell) "white"))))
                              (range (vector-length row))
                              (vector->list row))))
                (range (vector-length grid))
                (vector->list grid)))))

(define (solve-p1 inp)
  (call-with-values
   (λ () (run-moves inp))
   (λ (final-pos grid)
     (apply +
            (map (λ (box-pos)
                   (+ (* 100 (vec-y box-pos)) (vec-x box-pos)))
                 (apply append
                        (map (λ (y row)
                               (map car 
                                    (filter (λ (cell)
                                              (is-box? (cdr cell)))
                                            (map (λ (x cell)
                                                   (cons (make-vec x y) cell))
                                                 (range (vector-length row))
                                                 (vector->list row)))))
                             (range (vector-length grid))
                             (vector->list grid))))))))

(define (solve-p2 inp)
  #f)

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format #f "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (display
   (format "The solution to part ~a is: ~a~%" part result)))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input-2) 10092)
  (report-and-compare-results 1 (solve-p1 test-input-2) #f))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp) 1451928)
    (report-and-compare-results 2 (solve-p2 inp) #f)))
