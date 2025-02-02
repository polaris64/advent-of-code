#lang racket

(require pict)
(require pict/color)

(define test-input "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
")

(define test-input-2 "AAAA
BBCD
BBCC
EEEC
")

(define test-input-3 "EEEEE
EXXXX
EEEEE
EXXXX
EEEEE
")

(define test-input-4 "AAAAAA
AAABBA
AAABBA
ABBAAA
ABBAAA
AAAAAA
")

(define test-input-5 "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (parse-input inp-str)
  (map string->list
       (filter (λ (v) (> (string-length v) 0))
               (string-split inp-str "\n"))))

(define (make-pos x y)
  (cons x y))

(define (pos-x pos) (car pos))

(define (pos-y pos) (cdr pos))

(define (get-cell grid pos)
  (if (or (< (pos-x pos) 0)
          (< (pos-y pos) 0)
          (>= (pos-y pos) (length grid))
          (>= (pos-x pos) (length (car grid))))
      #f
      (list-ref (list-ref grid (pos-y pos)) (pos-x pos))))

(define (make-cell pos type)
  (cons pos type))

(define (cell-pos cell) (car cell))

(define (cell-type cell) (cdr cell))

(define (extract-cells grid)
  (apply append 
         (map (λ (y row)
                (map (λ (x cell)
                       (cons (make-pos x y) cell))
                     (range (length row))
                     row))
              (range (length grid))
              grid)))

(define (get-neighbours cell cells)
  (let ((lc (make-cell (make-pos (- (pos-x (cell-pos cell)) 1)
                                 (pos-y (cell-pos cell)))
                       (cell-type cell)))
        (rc (make-cell (make-pos (+ (pos-x (cell-pos cell)) 1)
                                 (pos-y (cell-pos cell)))
                       (cell-type cell)))
        (uc (make-cell (make-pos (pos-x (cell-pos cell))
                                 (- (pos-y (cell-pos cell)) 1))
                       (cell-type cell)))
        (dc (make-cell (make-pos (pos-x (cell-pos cell))
                                 (+ (pos-y (cell-pos cell)) 1))
                       (cell-type cell))))
    (filter (λ (neighbour)
              (member neighbour cells))
            (list lc rc uc dc))))

(define (extract-region visited cells)
  (define region (make-hash))
  (define (iter cell)
    (let* ((neighbours (filter (λ (nc)
                                 (not (hash-ref visited nc #f)))
                               (get-neighbours cell cells))))
      (when (not (null? neighbours))
        (for-each (λ (n)
                    (hash-set! region n #t)
                    (hash-set! visited n #t))
                  neighbours)
        (for-each iter neighbours))))
  (hash-set! region (car cells) #t)
  (hash-set! visited (car cells) #t)
  (iter (car cells))
  (map car (hash->list region)))

(define (extract-regions visited cells)
  (define loc-visited (if visited visited (make-hash)))
  (if (null? cells)
      '()
      (let ((region (extract-region loc-visited cells)))
        (cons region
              (extract-regions loc-visited
                               (filter (λ (c) (not (hash-ref loc-visited c #f)))
                                       cells))))))

(define (get-edge-count grid pos)
  (let ((cell (get-cell grid pos))
        (neighbours (list (make-pos (- (pos-x pos) 1) (pos-y pos))
                          (make-pos (+ (pos-x pos) 1) (pos-y pos))
                          (make-pos (pos-x pos) (- (pos-y pos) 1))
                          (make-pos (pos-x pos) (+ (pos-y pos) 1)))))
    (apply +
           (map (λ (p2) (if (eq? (get-cell grid p2) cell) 0 1)) neighbours))))

(define (make-span p1 p2 direction)
  (cons (cons p1 p2) direction))

(define (span-start span)
  (caar span))

(define (span-end span)
  (cdar span))

(define (span-direction span)
  (cdr span))

(define (copy-span span)
  (make-span (span-start span) (span-end span) (span-direction span)))

(define (direction-from-offsets s e)
  (cond ((and (equal? (cons 0 0) s) (equal? (cons 0 1) e)) 'L)
        ((and (equal? (cons 1 0) s) (equal? (cons 1 1) e)) 'R)
        ((and (equal? (cons 0 0) s) (equal? (cons 1 0) e)) 'T)
        ((and (equal? (cons 0 1) s) (equal? (cons 1 1) e)) 'B)))

(define (get-edges grid pos)
  (let ((cell (get-cell grid pos))
        (neighbours (list (cons (make-pos (- (pos-x pos) 1) (pos-y pos))
                                (cons (cons 0 0) (cons 0 1)))
                          (cons (make-pos (+ (pos-x pos) 1) (pos-y pos))
                                (cons (cons 1 0) (cons 1 1)))
                          (cons (make-pos (pos-x pos) (- (pos-y pos) 1))
                                (cons (cons 0 0) (cons 1 0)))
                          (cons (make-pos (pos-x pos) (+ (pos-y pos) 1))
                                (cons (cons 0 1) (cons 1 1))))))
    (filter identity
            (map (λ (p2)
                   (if (not (eq? (get-cell grid (car p2)) cell))
                       (let ((p2-pos (car p2))
                             (offset-s (cadr p2))
                             (offset-e (cddr p2)))
                         (make-span (make-pos (+ (pos-x pos) (car offset-s))
                                              (+ (pos-y pos) (cdr offset-s)))
                                    (make-pos (+ (pos-x pos) (car offset-e))
                                              (+ (pos-y pos) (cdr offset-e)))
                                    (direction-from-offsets offset-s offset-e)))
                       #f))
                 neighbours))))

(define (get-region-edges grid cells)
  (define (iter spans cells)
    (if (null? cells)
        spans
        (let ((edges (get-edges grid (cell-pos (car cells)))))
          (iter (append spans edges) (cdr cells)))))
  (iter '() cells))

(define (extend-span span all-spans)
  (define new-span (copy-span span))
  (define (extend-right)
    (let ((test-span (make-span (span-end new-span)
                                (make-pos (+ 1 (pos-x (span-end new-span)))
                                          (pos-y (span-end new-span)))
                                (span-direction span))))
      (when (hash-ref all-spans test-span #f)
        (hash-set! all-spans test-span #f)
        (set! new-span (make-span (span-start new-span)
                                  (span-end test-span)
                                  (span-direction new-span)))
        (extend-right))))
  (define (extend-left)
    (let ((test-span (make-span (make-pos (- (pos-x (span-start new-span)) 1)
                                          (pos-y (span-start new-span)))
                                (span-start new-span)
                                (span-direction span))))
      (when (hash-ref all-spans test-span #f)
        (hash-set! all-spans test-span #f)
        (set! new-span (make-span (span-start test-span)
                                  (span-end new-span)
                                  (span-direction new-span)))
        (extend-left))))
  (define (extend-down)
    (let ((test-span (make-span (span-end new-span)
                                (make-pos (pos-x (span-end new-span))
                                          (+ (pos-y (span-end new-span)) 1))
                                (span-direction span))))
      (when (hash-ref all-spans test-span #f)
        (hash-set! all-spans test-span #f)
        (set! new-span (make-span (span-start new-span)
                                  (span-end test-span)
                                  (span-direction new-span)))
        (extend-down))))
  (define (extend-up)
    (let ((test-span (make-span (make-pos (pos-x (span-start new-span))
                                          (- (pos-y (span-start new-span)) 1))
                                (span-start new-span)
                                (span-direction span))))
      (when (hash-ref all-spans test-span #f)
        (hash-set! all-spans test-span #f)
        (set! new-span (make-span (span-start test-span)
                                  (span-end new-span)
                                  (span-direction new-span)))
        (extend-up))))

  ;; Extend horizontal spans left and right
  (when (= (pos-y (span-start new-span)) (pos-y (span-end new-span)))
    (extend-right)
    (extend-left))

  ;; Extend vertical spans up and down
  (when (= (pos-x (span-start new-span)) (pos-x (span-end new-span)))
    (extend-up)
    (extend-down))

  new-span)

(define (merge-edges spans)
  (define span-table (make-hash))
  (for-each (λ (span) (hash-set! span-table span #t)) spans)

  (define (iter rest)
    (if (null? rest)
        '()
        (if (hash-ref span-table (car rest) #f)
            (cons (extend-span (car rest) span-table) (iter (cdr rest)))
            (iter (cdr rest)))))

  (iter spans))

(define (get-region-perimiter grid region)
  (apply +
         (map (λ (cell)
                (get-edge-count grid (cell-pos cell)))
              region)))

(define (get-region-perimiters grid cells)
  (map (λ (region) (get-region-perimiter grid region)) cells))

(define (get-region-merged-perimiters grid regions)
  (map (λ (region)
         (length (merge-edges (get-region-edges grid region))))
       regions))

(define (get-region-areas regions)
  (map length regions))

(define (solve-p1 inp)
  (let ((inp (if (string? inp) (parse-input inp) inp)))
    (apply +
           (map (λ (p a) (* p a))
                (get-region-perimiters inp (extract-regions #f (extract-cells inp)))
                (get-region-areas (extract-regions #f (extract-cells inp)))))))

(define (solve-p2 inp)
  (let* ((inp (if (string? inp) (parse-input inp) inp))
         (regions (extract-regions #f (extract-cells inp))))
    (apply +
           (map (λ (p a) (* p a))
                (get-region-merged-perimiters inp regions)
                (get-region-areas regions)))))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format #f "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (printf "The solution to part ~a is: ~a~%" part result))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input) 1930)
  (report-and-compare-results 2 (solve-p2 test-input) 1206))

(define (run)
  (let ((inp (parse-input (read-input))))
    (report-and-compare-results 1 (solve-p1 inp) 1456082)
    (report-and-compare-results 2 (solve-p2 inp) 872382)))

(define (visualise-grid grid edges)
  (let ((scale 20))
    (apply lt-superimpose
           (apply vl-append
                  (map (λ (row)
                         (apply htl-append (map (λ (cell)
                                                  (cc-superimpose
                                                   (rectangle scale scale #:border-color "white")
                                                   (text (string cell))))
                                                row)))
                       grid))
           (map (λ (edge)
                  (cond ((eq? 'T (span-direction edge))
                         (translate
                          (red
                           (hline (- (* scale (pos-x (span-end edge)))
                                     (* scale (pos-x (span-start edge))))
                                  0))
                          (* scale (pos-x (span-start edge)))
                          (* scale (pos-y (span-start edge)))))
                        ((eq? 'B (span-direction edge))
                         (translate
                          (green
                           (hline (- (* scale (pos-x (span-end edge)))
                                     (* scale (pos-x (span-start edge))))
                                  0))
                          (* scale (pos-x (span-start edge)))
                          (* scale (pos-y (span-start edge)))))
                        ((eq? 'L (span-direction edge))
                         (translate
                          (blue
                           (vline 0
                                  (- (* scale (pos-y (span-end edge)))
                                     (* scale (pos-y (span-start edge))))))
                          (* scale (pos-x (span-start edge)))
                          (* scale (pos-y (span-start edge)))))
                        ((eq? 'R (span-direction edge))
                         (translate
                          (orange
                           (vline 0
                                  (- (* scale (pos-y (span-end edge)))
                                     (* scale (pos-y (span-start edge))))))
                          (* scale (pos-x (span-start edge)))
                          (* scale (pos-y (span-start edge)))))))
                edges))))

;; (let ((grid (parse-input test-input)))
;;   (let ((edges (merge-edges (get-region-edges grid (list-ref (extract-regions #f (extract-cells grid)) 3)))))
;;     (visualise-grid grid edges)))
