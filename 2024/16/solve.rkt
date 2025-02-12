#lang racket

(require data/heap)
(require pict)

(define test-input-1 "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
")

(define test-input-2 "#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################
")

(define test-input-3 "#######
#....E#
#.##.##
#S...##
#######
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (make-vec x y) (cons x y))

(define (vec-x v) (car v))

(define (vec-y v) (cdr v))

(define (parse-input inp-str)
  (define (find-char-pos grid ch)
    (car (dropf (map (λ (y row)
                       (cons (index-where row (λ (cell) (eq? cell ch)))
                             y))
                     (range (length grid))
                     grid)
                (compose not car))))

  (let ((grid (map string->list (string-split inp-str "\n"))))
    (values
     (find-char-pos grid #\S)
     (find-char-pos grid #\E)
     (list->vector (map (λ (row)
                          (list->vector
                           (map (λ (cell)
                                  (if (or (eq? cell #\S) (eq? cell #\E))
                                      #\.
                                      cell))
                                row)))
                        grid)))))

(define (visualise-grid grid
                        start-pos
                        end-pos
                        #:path [path #f]
                        #:path-colour [path-colour "pink"]
                        #:prev [prev #f]
                        #:distances [distances #f])
  (let ((scale (if distances 30 5)))
    (apply vl-append
           (map (λ (y row)
                  (apply hc-append
                         (map (λ (x cell)
                                (let ((cell-pict (colorize
                                                  (filled-rectangle scale scale)
                                                  (cond [(eq? cell #\#) "black"]
                                                        [(equal? (make-vec x y) start-pos) "green"]
                                                        [(equal? (make-vec x y) end-pos) "red"]
                                                        [(and path (member (make-vec x y) path)) path-colour]
                                                        [else "white"]))))
                                  (if distances
                                      (cc-superimpose cell-pict
                                                      (text (format "~a"
                                                                    (hash-ref distances
                                                                              (make-vec x y)
                                                                              ""))
                                                            'default
                                                            8))
                                      cell-pict)))
                              (range (vector-length row))
                              (vector->list row))))
                (range (vector-length grid))
                (vector->list grid)))))

(define (get-grid-cell grid pos)
  (if (or (< (vec-y pos) 0)
          (>= (vec-y pos) (vector-length grid)))
      #f
      (let ((row (vector-ref grid (vec-y pos))))
        (if (or (< (vec-x pos) 0)
                (>= (vec-x pos) (vector-length row)))
            #f
            (vector-ref row (vec-x pos))))))

(define (get-leaves tree)
  (cond ((= (length (filter (λ (v) (not (list? v))) tree))
            (length tree))
         (list tree))
        (else (apply append (map get-leaves tree)))))

(define (reconstruct-path came-from end-pos start-pos)
  (define (iter path next)
    (let ((new-next (hash-ref came-from next #f)))
      (if (or (equal? new-next next) (not next))
          path
          (iter (cons next path)
                new-next))))
  (iter (list end-pos)
        (hash-ref came-from end-pos #f)))

(define (dfs graph start end)
  (define (get-neighbours node)
    (map cdr (filter (λ (edge) (equal? (car edge) node)) graph)))

  (define (iter path node visited)
    (let ((neighbours (filter (λ (n) (not (hash-ref visited n #f)))
                              (get-neighbours node))))
      (filter (λ (path) (> (length path) 0))
              (map (λ (neighbour)
                     (if (equal? neighbour end)
                         (reverse (cons neighbour path))
                         (iter (cons neighbour path)
                               neighbour
                               (hash-set visited neighbour #t))))
                   neighbours))))

  (map reverse
       (get-leaves (iter (list start)
                         start
                         (hash start #t)))))

(define (prev-to-edges prev)
  (apply append
         (map (λ (k)
                (let ((l (hash-keys (hash-ref prev k (hash)))))
                  (if (> (length l) 1)
                      (map (λ (v) (cons k v)) l)
                      (list (cons k (car l))))))
              (hash-keys prev))))

(define (make-neighbour pos cost heading) (list pos cost heading))

(define (neighbour-pos v) (car v))

(define (neighbour-cost v) (cadr v))

(define (neighbour-heading v) (caddr v))

(define (get-neighbours pos heading)
  (filter neighbour-cost
          (list (make-neighbour (make-vec (- (vec-x pos) 1) (vec-y pos))
                                (cond [(eq? heading 'W) 1]
                                      [(eq? heading 'E) #f]
                                      [else 1001])
                                'W)
                (make-neighbour (make-vec (+ (vec-x pos) 1) (vec-y pos))
                                (cond [(eq? heading 'E) 1]
                                      [(eq? heading 'W) #f]
                                      [else 1001])
                                'E)
                (make-neighbour (make-vec (vec-x pos) (- (vec-y pos) 1))
                                (cond [(eq? heading 'N) 1]
                                      [(eq? heading 'S) #f]
                                      [else 1001])
                                'N)
                (make-neighbour (make-vec (vec-x pos) (+ (vec-y pos) 1))
                                (cond [(eq? heading 'S) 1]
                                      [(eq? heading 'N) #f]
                                      [else 1001])
                                'S))))

(define (a-star grid start-pos end-pos h)
  (define (make-open-set-entry pos heading) (cons pos heading))
  (define (open-set-pos v) (car v))
  (define (open-set-heading v) (cdr v))

  (let* ((came-from (make-hash))
         (gscores (make-hash (list (cons start-pos 0))))
         (fscores (make-hash (list (cons start-pos (h start-pos)))))
         (open-set-fscore (λ (v) (hash-ref fscores
                                           (open-set-pos v)
                                           'INF)))
         (open-set (make-heap (λ (a b) (let ((da (open-set-fscore a))
                                             (db (open-set-fscore b)))
                                         (cond ((and (eq? da 'INF) (eq? db 'INF)) da)
                                               ((and (eq? da 'INF) (not (eq? db 'INF))) db)
                                               ((and (not (eq? da 'INF)) (eq? db 'INF)) da)
                                               (else (<= da db))))))))

    (define (iter open-set)
      (if (= 0 (heap-count open-set))
          #f
          (let ((current (heap-min open-set)))
            (if (equal? (open-set-pos current) end-pos)
                (reconstruct-path came-from end-pos start-pos)
                (begin
                  (heap-remove-min! open-set)
                  (for-each (λ (neighbour)
                              (let ((cell (get-grid-cell grid (neighbour-pos neighbour))))
                                (when (eq? cell #\.)
                                  (let ((tentative-g-score (+ (open-set-fscore current)
                                                              (neighbour-cost neighbour)))
                                        (existing-score (hash-ref fscores
                                                                  (neighbour-pos neighbour)
                                                                  'INF)))

                                    (when (or (eq? 'INF existing-score)
                                              (< tentative-g-score existing-score))
                                      (begin
                                        (hash-set! came-from
                                                   (neighbour-pos neighbour)
                                                   (open-set-pos current))
                                        (hash-set! gscores
                                                   (neighbour-pos neighbour)
                                                   tentative-g-score)
                                        (hash-set! fscores
                                                   (neighbour-pos neighbour)
                                                   (+ tentative-g-score
                                                      (h (neighbour-pos neighbour))))
                                        (when (= 0
                                                 (sequence-length (sequence-filter
                                                                   (λ (v)
                                                                     (equal? v (neighbour-pos neighbour)))
                                                                   (in-heap open-set))))
                                          (heap-add! open-set
                                                     (make-open-set-entry 
                                                      (neighbour-pos neighbour)
                                                      (neighbour-heading neighbour))))))))))
                            (get-neighbours (open-set-pos current)
                                            (open-set-heading current)))

                  (iter open-set))))))

    (heap-add! open-set (make-open-set-entry start-pos 'E))
    (iter open-set)))

(define (dijkstra grid start-pos end-pos)
  (define (make-open-set-entry pos heading) (cons pos heading))
  (define (open-set-pos v) (car v))
  (define (open-set-heading v) (cdr v))

  (let* ((prev (make-hash))
         (distances (make-hash))
         (queue (make-heap (λ (a b)
                             (let ((da (hash-ref distances (open-set-pos a) 'INF))
                                   (db (hash-ref distances (open-set-pos b) 'INF)))
                               (cond ((and (eq? da 'INF) (eq? db 'INF)) da)
                                     ((and (eq? da 'INF) (not (eq? db 'INF))) db)
                                     ((and (not (eq? da 'INF)) (eq? db 'INF)) da)
                                     (else (<= da db))))))))

    (define (iter)
      (if (= 0 (heap-count queue))

          (values (dfs (prev-to-edges prev) end-pos start-pos)
                  prev
                  distances)

          (let ((next (heap-min queue)))

            (heap-remove-min! queue)

            (for-each (λ (neighbour)
                        (let ((dist (+ (hash-ref distances (open-set-pos next) 0)
                                       (neighbour-cost neighbour)))
                              (existing-dist (hash-ref distances (neighbour-pos neighbour) 'INF)))

                          ;; Skip neighbour if its cost is greater
                          ;; than the end position's cost (if found)
                          (when (or (not (hash-ref distances end-pos #f))
                                    (<= dist (hash-ref distances end-pos)))

                            ;; Add a path from this neighbour back to
                            ;; the current cell if the distance is
                            ;; less or if the distance mod 1000 (to
                            ;; ignore turns) is the same
                            (when (or (eq? 'INF existing-dist)
                                      (< dist existing-dist)
                                      (= (modulo dist 1000) (modulo existing-dist 1000)))
                              (let ((curr-prev-list (hash-ref prev (neighbour-pos neighbour) (make-hash))))
                                (hash-set! curr-prev-list (open-set-pos next) #t)
                                (hash-set! prev (neighbour-pos neighbour) curr-prev-list)))

                            ;; If the distance is less then store the
                            ;; distance and add the neighbour to the
                            ;; queue
                            (when (or (eq? 'INF existing-dist)
                                      (< dist existing-dist))
                              (hash-set! distances (neighbour-pos neighbour) dist)
                              (heap-add! queue
                                         (make-open-set-entry (neighbour-pos neighbour)
                                                              (neighbour-heading neighbour)))))))

                      (filter (λ (neighbour)
                                (eq? #\. (get-grid-cell grid (neighbour-pos neighbour))))
                              (get-neighbours (open-set-pos next) (open-set-heading next))))

            (iter))))

    (hash-set! distances start-pos 0)
    (heap-add! queue (make-open-set-entry start-pos 'E))

    (iter)))

(define (visualise-paths inp)
  (let-values ([(start-pos end-pos grid) (parse-input inp)])
    (let-values (((paths prev distances) (dijkstra grid start-pos end-pos)))
      (printf "Paths: ~a\n" paths)
      (printf "Prev: ~a\n" prev)
      (apply vc-append
             (map (λ (path)
                    (visualise-grid grid
                                    start-pos
                                    end-pos
                                    #:path path
                                    #:prev prev
                                    #:distances distances))
                  paths)))))

(define (get-unique-tiles paths)
  (let ((tiles (make-hash)))
    (for-each (λ (path)
                (for-each (λ (tile)
                            (hash-set! tiles tile #t)) path))
              paths)
    (hash-keys tiles)))

(define (get-heading p1 p2)
  (cond ((and (eq? (vec-x p1) (vec-x p2))
              (< (- (vec-y p2) (vec-y p1)) 0)) 'N)
        ((and (eq? (vec-x p1) (vec-x p2))
              (> (- (vec-y p2) (vec-y p1)) 0)) 'S)
        ((and (eq? (vec-y p1) (vec-y p2))
              (< (- (vec-x p2) (vec-x p1)) 0)) 'W)
        ((and (eq? (vec-y p1) (vec-y p2))
              (> (- (vec-x p2) (vec-x p1)) 0)) 'E)
        (else (error 'get-heading "Unable to determine direction: ~a, ~a" p1 p2))))

(define (get-path-score path heading)
  (define (iter heading prev-pos path)
    (if (null? path)
        0
        (let ((new-heading (get-heading prev-pos (car path))))
          (+ (if (eq? heading new-heading)
                 1
                 1001)
             (iter new-heading (car path) (cdr path))))))
  (if (<= (length path) 1)
      #f
      (iter heading (car path) (cdr path))))

(define (solve-p1 inp)
  (let-values ([(start-pos end-pos grid) (parse-input inp)])
    (let ((path (a-star grid
                        start-pos
                        end-pos
                        (λ (p)
                          (abs (- (vec-x end-pos) (vec-x p)))
                          (abs (- (vec-y end-pos) (vec-y p)))))))
      (get-path-score path 'E))))

(define (solve-p2 inp)
  (let-values ([(start-pos end-pos grid) (parse-input inp)])
    (let-values (((paths prev distances) (dijkstra grid start-pos end-pos)))
      (let* ((scores (map (λ (path) (cons (get-path-score path 'E) path)) paths))
             (min-score (argmin car scores))
             (min-paths (map cdr (filter (λ (path) (= (car path) (car min-score))) scores))))
          (length (get-unique-tiles min-paths))))))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (display
   (format "The solution to part ~a is: ~a~%" part result)))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results "1 (input 1)" (solve-p1 test-input-1) 7036)
  (report-and-compare-results "1 (input 2)" (solve-p1 test-input-2) 11048)
  (report-and-compare-results "2 (input 1)" (solve-p2 test-input-1) 45)
  (report-and-compare-results "2 (input 2)" (solve-p2 test-input-2) 64))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp) 102460)
    (report-and-compare-results 2 (solve-p2 inp) 527)))
