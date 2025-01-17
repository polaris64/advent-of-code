(use-modules (ice-9 textual-ports))

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (get-string-all port))))

(define test-input 
  "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............
")

(define (range s e)
  (if (= s e)
      '()
      (cons s (range (1+ s) e))))

(define (parse-input inp)
  (let ((inp-lines (filter (λ (l) (> (string-length l) 0)) (string-split inp #\newline)))
        (antennas (make-hash-table)))
    (for-each (λ (y row)
                (for-each (λ (x cell)
                            (when (not (eq? cell #\.))
                              (hash-set! antennas (cons y x) cell)))
                          (range 0 (string-length row))
                          (string->list row)))
              (range 0 (length inp-lines)) inp-lines)
    (values antennas
            (cons (length inp-lines) (string-length (car inp-lines))))))

(define (antenna-pos v)
  (car v))

(define (antenna-type v)
  (cdr v))

(define (pos-x p) (cdr p))

(define (pos-y p) (car p))

(define (get-antinodes grid-size a1 a2)
  (let ((antinodes (make-hash-table)))
    (when (eq? (antenna-type a1) (antenna-type a2))
      (let* ((diff (cons (- (pos-y (antenna-pos a1))
                            (pos-y (antenna-pos a2)))
                         (- (pos-x (antenna-pos a1))
                            (pos-x (antenna-pos a2)))))
             (ant1 (cons (+ (pos-y (antenna-pos a1)) (pos-y diff))
                         (+ (pos-x (antenna-pos a1)) (pos-x diff))))
             (ant2 (cons (- (pos-y (antenna-pos a2)) (pos-y diff))
                         (- (pos-x (antenna-pos a2)) (pos-x diff)))))
        (for-each (λ (a)
                    (when (and (>= (pos-x a) 0)
                               (>= (pos-y a) 0)
                               (< (pos-x a) (cdr grid-size))
                               (< (pos-y a) (car grid-size)))
                      (hash-set! antinodes a #t)))
                  (list ant1 ant2))))
    (map car (hash-map->list cons antinodes))))

(define (get-antinodes-with-harmonics grid-size a1 a2)
  (let ((antinodes (make-hash-table)))
    (define (add-harmonics! diff op pos)
      (if (and (>= (pos-x pos) 0)
               (>= (pos-y pos) 0)
               (< (pos-x pos) (cdr grid-size))
               (< (pos-y pos) (car grid-size)))
          (begin
            (hash-set! antinodes pos #t)
            (add-harmonics! diff op (cons (op (pos-y pos) (pos-y diff))
                                          (op (pos-x pos) (pos-x diff)))))))

    (when (eq? (antenna-type a1) (antenna-type a2))
      (let ((diff (cons (- (pos-y (antenna-pos a1))
                           (pos-y (antenna-pos a2)))
                        (- (pos-x (antenna-pos a1))
                           (pos-x (antenna-pos a2))))))
        (add-harmonics! diff + (antenna-pos a1))
        (add-harmonics! diff - (antenna-pos a1))
        (add-harmonics! diff + (antenna-pos a2))
        (add-harmonics! diff - (antenna-pos a2))))

    (map car (hash-map->list cons antinodes))))

(define (find-antinodes grid-size antennas harmonics?)
  (let ((antinodes (make-hash-table))
        (antenna-list (hash-map->list cons antennas)))
    (define (iter rest-antennas)
      (when (not (null? rest-antennas))
        (for-each (λ (antenna)
                    (for-each (λ (antinode)
                                (hash-set! antinodes antinode (1+ (hash-ref antinodes antinode 0))))
                              ((if harmonics? get-antinodes-with-harmonics get-antinodes) grid-size (car rest-antennas) antenna)))
                  (cdr rest-antennas))
        (iter (cdr rest-antennas))))
    (iter antenna-list)
    antinodes))

(define (get-all-antinodes inp harmonics?)
  (call-with-values
      (λ () (parse-input inp))
    (λ (antennas grid-size)
      (hash-map->list cons (find-antinodes grid-size antennas harmonics?)))))

(define (solve-p1 inp)
  (length (get-all-antinodes inp #f)))

(define (solve-p2 inp)
  (length (get-all-antinodes inp #t)))

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
  (report-and-compare-results 1 (solve-p1 test-input) 14)
  (report-and-compare-results 2 (solve-p2 test-input) 34))

(define (run)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 303)
    (report-and-compare-results 2 (solve-p2 inp-str) 1045)))
