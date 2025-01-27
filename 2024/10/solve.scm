(use-modules (ice-9 textual-ports))

(define test-input "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (get-string-all port))))

(define (parse-input input-str)
  (map (λ (v) (map (compose string->number string) (string->list v)))
       (filter (λ (v) (> (string-length v) 0))
               (string-split input-str #\newline))))

(define (is-trailhead? v)
  (= v 0))

(define (pos-x p)
  (car p))

(define (pos-y p)
  (cdr p))

(define (make-pos x y)
  (cons x y))

(define (get-map-cell grid pos)
  (if (or (< (pos-x pos) 0)
          (< (pos-y pos) 0)
          (>= (pos-y pos) (length grid))
          (>= (pos-x pos) (length (car grid))))
      #f
      (list-ref (list-ref grid
                          (pos-y pos))
                (pos-x pos))))

(define (trace-trailhead grid pos)
  (define summits (make-hash-table))

  (define (get-score height pos)
    (let ((cell (get-map-cell grid pos)))
      (cond ((and cell (= cell 9) (= height 9))
             (begin
               (hash-set! summits pos (1+ (hash-ref summits pos 0)))
               1))
            ((or (not cell) (not (= cell height))) 0)
            (else (sum-neighbours height pos)))))

  (define (sum-neighbours height pos)
    (+ (get-score (1+ height) (make-pos (1- (pos-x pos)) (pos-y pos)))
       (get-score (1+ height) (make-pos (1+ (pos-x pos)) (pos-y pos)))
       (get-score (1+ height) (make-pos (pos-x pos) (1- (pos-y pos))))
       (get-score (1+ height) (make-pos (pos-x pos) (1+ (pos-y pos))))))

  (if (not (is-trailhead? (get-map-cell grid pos)))
      (values 0 '())
      (values (sum-neighbours 0 pos)
              (hash-map->list cons summits))))

(define (get-summit-count-for-trailhead grid pos)
  (call-with-values
      (λ () (trace-trailhead grid pos))
    (λ (total summits)
      (length summits))))

(define (get-distinct-routes-for-trailhead grid pos)
  (call-with-values
      (λ () (trace-trailhead grid pos))
    (λ (total summits)
      total)))

(define (sum-trailhead-scores grid scoring-fn)
  (apply +
         (map (λ (y row)
                (apply +
                       (map (λ (x)
                              (scoring-fn grid (make-pos x y)))
                            (iota (length row) 0))))
              (iota (length grid) 0) grid)))

(define (solve-p1 inp)
  (sum-trailhead-scores (parse-input inp) get-summit-count-for-trailhead))

(define (solve-p2 inp)
  (sum-trailhead-scores (parse-input inp) get-distinct-routes-for-trailhead))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format #f "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (format (current-output-port) "The solution to part ~a is: ~a~%" part result))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input) 36)
  (report-and-compare-results 2 (solve-p2 test-input) 81))

(define (run)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 629)
    (report-and-compare-results 2 (solve-p2 inp-str) 1242)))
