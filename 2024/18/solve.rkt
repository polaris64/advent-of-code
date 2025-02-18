#lang racket

(require data/heap
         pict)

(define test-input "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (parse-input inp-str)
  (map (λ (line) (let ((parts (string-split line ",")))
                   (cons (cons (string->number (car parts)) (string->number (cadr parts))) #t)))
       (string-split inp-str "\n")))

(define (make-cell-hash cells num)
  (make-hash (take cells num)))

(define (make-vec x y) (cons x y))

(define (vec-x v) (car v))

(define (vec-y v) (cdr v))

(define (make-neighbour pos blocked) (cons pos blocked))

(define (neighbour-pos n) (car n))

(define (neighbour-blocked n) (cdr n))

(define (get-neighbours cells pos size)
  (map (λ (p)
         (make-neighbour p (hash-ref cells p #f)))
       (filter (λ (p)
                 (and (>= (vec-x p) 0)
                      (>= (vec-y p) 0)
                      (<= (vec-x p) (vec-x size))
                      (<= (vec-y p) (vec-y size))))
               (list (make-vec (- (vec-x pos) 1) (vec-y pos))
                     (make-vec (+ (vec-x pos) 1) (vec-y pos))
                     (make-vec (vec-x pos) (- (vec-y pos) 1))
                     (make-vec (vec-x pos) (+ (vec-y pos) 1))))))

(define (reconstruct-path came-from end-pos)
  (define (iter path next)
    (let ((new-next (hash-ref came-from next #f)))
      (if (or (not next) (equal? new-next next))
          path
          (iter (cons next path)
                new-next))))
  (iter (list end-pos)
        (hash-ref came-from end-pos #f)))

(define (a-star cells start-pos end-pos grid-size h)
  (let* ((came-from (make-hash))
         (gscores (make-hash (list (cons start-pos 0))))
         (fscores (make-hash (list (cons start-pos (h start-pos)))))
         (open-set-fscore (λ (v) (hash-ref fscores v 'INF)))
         (open-set (make-heap (λ (a b) (let ((da (open-set-fscore a))
                                             (db (open-set-fscore b)))
                                         (cond ((and (eq? da 'INF) (eq? db 'INF)) da)
                                               ((and (eq? da 'INF) (not (eq? db 'INF))) db)
                                               ((and (not (eq? da 'INF)) (eq? db 'INF)) da)
                                               (else (<= da db))))))))

    (define (iter)
      (if (= 0 (heap-count open-set))
          #f
          (let ((current (heap-min open-set)))
            (if (equal? current end-pos)
                (reconstruct-path came-from
                                  end-pos)
                (begin
                  (heap-remove-min! open-set)
                  (for-each (λ (neighbour)
                              (let ((tentative-g-score (+ (hash-ref gscores current 0) 1))
                                    (existing-score (hash-ref gscores neighbour 'INF)))

                                (when (or (eq? 'INF existing-score)
                                          (< tentative-g-score existing-score))
                                  (begin
                                    (hash-set! came-from neighbour current)
                                    (hash-set! gscores neighbour tentative-g-score)
                                    (hash-set! fscores neighbour (+ tentative-g-score (h neighbour)))
                                    (when (= 0
                                             (sequence-length (sequence-filter
                                                               (λ (v)
                                                                 (equal? v (neighbour-pos neighbour)))
                                                               (in-heap open-set))))
                                      (heap-add! open-set neighbour))))))
                            (map neighbour-pos
                                 (filter (compose not neighbour-blocked)
                                         (get-neighbours cells current grid-size))))

                  (iter))))))

    (heap-add! open-set start-pos)
    (iter)))

(define (visualise-grid cells start-pos end-pos path)
  (let ((scale 10)
        (min-pos (make-vec (apply min (map vec-x (hash-keys cells)))
                           (apply min (map vec-y (hash-keys cells)))))
        (max-pos (make-vec (apply max (map vec-x (hash-keys cells)))
                           (apply max (map vec-y (hash-keys cells))))))
    (apply vc-append
           (map (λ (y)
                  (apply hc-append
                         (map (λ (x)
                                (cc-superimpose
                                 (colorize
                                  (filled-rectangle scale scale)
                                  (cond ((equal? (make-vec x y) start-pos) "green")
                                        ((equal? (make-vec x y) end-pos) "red")
                                        ((hash-ref cells (make-vec x y) #f) "black")
                                        ((member (make-vec x y) path) "blue")
                                        (else "white")))
                                 (rectangle scale scale)))
                              (range (vec-x min-pos) (+ 1 (vec-x max-pos)))))
                  )
                (range (vec-y min-pos) (+ 1 (vec-y max-pos)))))))

(define (solve-p1 inp num-bytes grid-size)
  (let ((cells (make-cell-hash (parse-input inp) num-bytes))
        (start-pos (make-vec 0 0))
        (end-pos grid-size))
    (let ((path (a-star cells
                        start-pos
                        end-pos
                        grid-size
                        (λ (p)
                          (abs (- (vec-x end-pos) (vec-x p)))
                          (abs (- (vec-y end-pos) (vec-y p)))))))
      ;; (printf "Path: ~a\n" path)
      ;; (printf "Path length: ~a\n" (- (length path) 1))
      ;; (visualise-grid cells start-pos end-pos (or path (list)))
      (- (length path) 1))
    ;; (visualise-grid cells start-pos end-pos (list))
    ))

(define (visualise-path inp num-bytes grid-size)
  (let ((cells (make-cell-hash (parse-input inp) num-bytes))
        (start-pos (make-vec 0 0))
        (end-pos grid-size))
    (let ((path (a-star cells
                        start-pos
                        end-pos
                        grid-size
                        (λ (p)
                          (abs (- (vec-x end-pos) (vec-x p)))
                          (abs (- (vec-y end-pos) (vec-y p)))))))
      (printf "Path: ~a\n" path)
      (printf "Path length: ~a\n" (if path (- (length path) 1) "n/a"))
      (visualise-grid cells start-pos end-pos (or path (list))))))

;; (visualise-path test-input 12 (make-vec 6 6))
;; (visualise-path (read-input) 1024 (make-vec 70 70))
;; (visualise-path (read-input) 2450 (make-vec 70 70))

(define (get-next-cell-on-path cells path offset)
  (let ((cells (list->vector cells))
        (path (apply hash (apply append (map (λ (v) (list v #t)) path)))))
    (define (iter idx max-len)
      (cond ((>= idx max-len) #f)
            ((hash-ref path (vector-ref cells idx) #f) idx)
            (else (iter (+ 1 idx) max-len))))
    (iter offset (vector-length cells))))

(define (binary-search start end test)
  (let* ((idx (+ start (truncate (/ (- end start) 2))))
         (res (test idx)))
    (cond ((and (= start end) res) start)
          ((and (= start end) (not res)) #f)
          (res (binary-search start (- idx 1) test))
          ((not res) (binary-search (+ idx 1) end test)))))

(define (get-first-blocking-byte-using-shortest-path inp num-bytes grid-size)
  (let* ((all-cells (map car inp))
         (start-pos (make-vec 0 0))
         (end-pos grid-size))

    (define (iter num-bytes)
      (let* ((cells (make-cell-hash inp num-bytes))
             (path (a-star cells
                           start-pos
                           end-pos
                           grid-size
                           (λ (p)
                             (abs (- (vec-x end-pos) (vec-x p)))
                             (abs (- (vec-y end-pos) (vec-y p)))))))
        (if (not path)
            (let ((byte-pos (car (list-ref inp (- num-bytes 1)))))
              (string-join (list (number->string (vec-x byte-pos))
                                 (number->string (vec-y byte-pos)))
                           ","))
            (let ((next-offset (get-next-cell-on-path all-cells
                                                      path
                                                      num-bytes)))
              (if (and next-offset (not (= next-offset num-bytes)))
                  (iter next-offset)
                  (iter (+ 1 num-bytes)))))))

    (iter num-bytes)))

(define (get-first-blocking-byte-using-binary-search inp num-bytes grid-size)
  (let* ((all-cells (map car inp))
         (start-pos (make-vec 0 0))
         (end-pos grid-size))

    (let ((res (binary-search num-bytes
                              (length all-cells)
                              (λ (idx)
                                (let ((cells (make-cell-hash inp idx)))
                                  (not
                                   (a-star cells
                                           start-pos
                                           end-pos
                                           grid-size
                                           (λ (p)
                                             (abs (- (vec-x end-pos) (vec-x p)))
                                             (abs (- (vec-y end-pos) (vec-y p)))))))))))
      (if res
          (let ((byte-pos (car (list-ref inp (- res 1)))))
            (string-join (list (number->string (vec-x byte-pos))
                               (number->string (vec-y byte-pos)))
                         ","))
          res))))

(define (solve-p2 inp num-bytes grid-size)

  ;; Works, but slower
  ;; (get-first-blocking-byte-using-shortest-path (parse-input inp)
  ;;                                              num-bytes
  ;;                                              grid-size)

  (get-first-blocking-byte-using-binary-search (parse-input inp)
                                               num-bytes
                                               grid-size))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (printf "The solution to part ~a is: ~a~%" part result))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input 12 (make-vec 6 6)) 22)
  (report-and-compare-results 2 (solve-p2 test-input 12 (make-vec 6 6)) "6,1"))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp 1024 (make-vec 70 70)) 272)
    (report-and-compare-results 2 (solve-p2 inp 1024 (make-vec 70 70)) "16,44")))
