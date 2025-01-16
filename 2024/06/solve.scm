(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input
  "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
")

(define (range s e)
  (if (= s e)
      '()
      (cons s (range (1+ s) e))))

(define (find-guard grid)
  (define (is-guard? cell)
    (eq? cell #\^))
  (let* ((row-idx (list-index (λ (row) (list-index is-guard? row)) grid))
         (cell-idx (list-index is-guard? (list-ref grid row-idx))))
    (cons row-idx cell-idx)))

(define (parse-input inp-str)
  (let* ((grid (map string->list (filter (λ (l) (> (string-length l) 0)) (string-split inp-str #\newline))))
         (guard-pos (find-guard grid)))
    (values (map (λ (row)
                   (map (λ (cell)
                          (if (eq? #\^ cell) #\. cell))
                        row))
                 grid)
            guard-pos)))

(define (display-grid grid guard-pos)
  (for-each (λ (row-idx row)
              (for-each (λ (cell-idx cell)
                          (if (equal? (cons row-idx cell-idx) guard-pos)
                              (display #\^)
                              (display cell)))
                        (range 0 (length row))row)
              (newline))
            (range 0 (length grid))
            grid))

(define (next-direction d)
  (cond ((eq? d 'U) 'R)
        ((eq? d 'R) 'D)
        ((eq? d 'D) 'L)
        ((eq? d 'L) 'U)
        (else (error "NEXT-DIRECTION: unknown direction" d))))

(define (pos-x p) (cdr p))

(define (pos-y p) (car p))

(define (update-pos grid pos dir)
  (let* ((new-pos (cond ((eq? dir 'U) (cons (1- (pos-y pos)) (pos-x pos)))
                        ((eq? dir 'D) (cons (1+ (pos-y pos)) (pos-x pos)))
                        ((eq? dir 'L) (cons (pos-y pos) (1- (pos-x pos))))
                        ((eq? dir 'R) (cons (pos-y pos) (1+ (pos-x pos))))))
         (new-pos-cell (list-ref (list-ref grid (pos-y new-pos)) (pos-x new-pos))))
    (if (eq? new-pos-cell #\#)
        (values pos (next-direction dir))
        (values new-pos dir))))

(define (store-pos! table pos)
  (hash-set! table pos (1+ (hash-ref table pos 0))))

(define (simulate grid initial-guard-pos)
  (define (iter guard-pos guard-direction visited-cells)
    (store-pos! visited-cells guard-pos)
    (if (or (= (pos-x guard-pos) 0)
            (= (pos-x guard-pos) (1- (length grid)))
            (= (pos-y guard-pos) 0)
            (= (pos-y guard-pos) (1- (length (car grid)))))
        visited-cells
        (call-with-values (λ () (update-pos grid guard-pos guard-direction))
          (λ (new-pos new-dir)
            (iter new-pos new-dir visited-cells)))))
  (iter initial-guard-pos 'U (make-hash-table (* (length grid) (length (car grid))))))

(define (store-pos-direction! table pos dir)
  (let ((key (cons dir pos)))
    (hash-set! table key (1+ (hash-ref table key 0)))))

(define (simulate-avoiding-loops grid initial-guard-pos)
  (define (iter guard-pos guard-direction visited-cells)
    (if (> (store-pos-direction! visited-cells guard-pos guard-direction) 1)
        (values #t visited-cells)
        (if (or (= (pos-x guard-pos) 0)
                (= (pos-x guard-pos) (1- (length grid)))
                (= (pos-y guard-pos) 0)
                (= (pos-y guard-pos) (1- (length (car grid)))))
            (values #f visited-cells)
            (call-with-values (λ () (update-pos grid guard-pos guard-direction))
              (λ (new-pos new-dir)
                (iter new-pos new-dir visited-cells))))))
  (iter initial-guard-pos
        'U
        (make-hash-table (* (length grid) (length (car grid))))))

(define (try-obstacles grid initial-guard-pos cells)
  (define (iter res rest-cells)
    (if (null? rest-cells)
        res
        (let ((trial-cell (car rest-cells)))
          (list-set! (list-ref grid (pos-y trial-cell)) (pos-x trial-cell) #\#)
          (call-with-values
              (λ () (simulate-avoiding-loops grid initial-guard-pos))
            (λ (loop? visited-cells)
              (list-set! (list-ref grid (pos-y trial-cell)) (pos-x trial-cell) #\.)
              (iter (if loop? (1+ res) res) (cdr rest-cells)))))))
  (iter 0 cells))

(define (solve-p1 inp)
  (call-with-values
      (λ () (parse-input inp))
    (λ (grid guard-pos)
      (length (hash-map->list cons (simulate grid guard-pos))))))

(define (solve-p2 inp)
  (call-with-values
      (λ () (parse-input inp))
    (λ (grid guard-pos)
      (let ((visited-cells (simulate grid guard-pos)))
        (try-obstacles grid
                       guard-pos
                       (filter (λ (v)
                                 (not (eq? guard-pos v)))
                               (map car (hash-map->list cons visited-cells))))))))

(solve-p1 test-input)
(solve-p2 test-input)

(call-with-input-file "input.txt"
  (lambda (port)
    (let* ((inp-str (get-string-all port))
           (p1 (solve-p1 inp-str))
           (p2 (solve-p2 inp-str)))
      (format (current-output-port) "The solution to part 1 is: ~a~%" p1)
      (if (not (= 4819 p1))
          (error "Part 1 solution is incorrect: 4819 !=" p1))
      (format (current-output-port) "The solution to part 2 is: ~a~%" p2)
      (if (not (= 1796 p2))
          (error "Part 2 solution is incorrect: 1796 !=" p2)))))
