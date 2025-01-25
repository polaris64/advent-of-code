(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input "2333133121414131402")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (get-string-all port))))

(define (range s e)
  (if (> s e)
      (error "RANGE: start should be less then end, but start/end are:" s e)
      (if (= s e)
          '()
          (cons s (range (1+ s) e)))))

(define (parse-input inp-str)
  (let ((res (fold (λ (v a)
                     (cons (not (car a))
                           (cons 
                            (if (car a) (cons v (cadr a)) (cadr a))
                            (if (not (car a)) (cons v (cddr a)) (cddr a)))))
                   (cons #t (cons (list) (list))) 
                   (reverse (map (compose string->number string)
                                 (filter (λ (ch) (not (eq? #\newline ch)))
                                         (string->list inp-str)))))))
    (values (cadr res)
            (cddr res))))

(define (extract-block-map file-blocks free-blocks)
  (define (iter block-map file-blocks free-blocks file-idx)
    (if (and (null? file-blocks) (null? free-blocks))
        block-map
        (iter (append block-map
                      (if (null? file-blocks) '() (make-list (car file-blocks) file-idx))
                      (if (null? free-blocks) '() (make-list (car free-blocks) #\.)))
              (if (null? file-blocks) '() (cdr file-blocks))
              (if (null? free-blocks) '() (cdr free-blocks))
              (1+ file-idx))))
  (list->vector (iter (list) file-blocks free-blocks 0)))

(define (parse-and-extract inp-str)
  (call-with-values (λ () (parse-input inp-str))
    (λ (file-blocks free-blocks)
      (extract-block-map file-blocks free-blocks))))

(define (empty-block? block)
  (eq? block #\.))

(define (defragmented? block-map start-idx)
  (let ((idx-start (get-first-empty-block-idx block-map start-idx)))
    (define (iter idx)
      (cond ((not (empty-block? (vector-ref block-map idx))) #f)
            ((= idx (1- (vector-length block-map))) #t)
            (else (iter (1+ idx)))))
    (iter idx-start)))

(define (defragment block-map)
  (define (iter pi1 pi2)
    (if (defragmented? block-map pi1)
        (vector->list block-map)
        (let ((i1 (get-first-empty-block-idx block-map pi1))
              (i2 (get-last-non-empty-block-idx block-map pi2)))
          (vector-set! block-map i1 (vector-ref block-map i2))
          (vector-set! block-map i2 #\.)
          (iter i1 i2))))
  (iter 0 (1- (vector-length block-map))))

(define (get-largest-file-id block-map)
  (let ((idx (get-last-non-empty-block-idx block-map (1- (vector-length block-map)))))
    (vector-ref block-map idx)))

(define (get-first-empty-block-idx block-map start-idx)
  (define (iter idx)
    (cond ((empty-block? (vector-ref block-map idx)) idx)
          ((>= idx (vector-length block-map)) #f)
          (else (iter (1+ idx)))))
  (iter start-idx))

(define (get-last-non-empty-block-idx block-map start-idx)
  (define (iter idx)
    (cond ((not (empty-block? (vector-ref block-map idx))) idx)
          ((= idx 0) #f)
          (else (iter (1- idx)))))
  (iter start-idx))

(define (get-file-start-idx block-map file-id)
  (define (iter idx)
    (cond ((= idx (vector-length block-map)) #f)
          ((eq? (vector-ref block-map idx) file-id) idx)
          (else (iter (1+ idx)))))
  (iter 0))

(define (get-span-length block-map start-idx v)
  (cond ((= (vector-length block-map) start-idx) 0)
        ((not (eq? (vector-ref block-map start-idx) v)) 0)
        (else (+ 1 (get-span-length block-map (1+ start-idx) v)))))

(define (get-span-with-length block-map start-idx v desired-length)
  (if (>= start-idx (vector-length block-map))
      #f
      (let ((l (get-span-length block-map start-idx v)))
        (if (>= l desired-length)
            start-idx
            (get-span-with-length block-map (+ start-idx (max l 1)) v desired-length)))))

(define (defragment-full-blocks block-map)
  (let ((largest-file-id (get-largest-file-id block-map)))
    (define (iter free-idx file-idx last-file-id)
      (if (< last-file-id 0)
          (vector->list block-map)
          (let* ((curr-file-length (get-span-length block-map file-idx last-file-id))
                 (free-block-idx (get-span-with-length block-map free-idx #\. curr-file-length)))

            ;; Swap file and free blocks
            (when (and free-block-idx (< free-block-idx file-idx))
              (for-each (λ (idx)
                          (vector-set! block-map (+ idx file-idx) #\.)
                          (vector-set! block-map (+ idx free-block-idx) last-file-id))
                        (range 0 curr-file-length)))

            (iter 0
                  (get-file-start-idx block-map (1- last-file-id))
                  (1- last-file-id)))))

    (iter (get-first-empty-block-idx block-map 0)
          (get-file-start-idx block-map largest-file-id)
          largest-file-id)))

(define (defragment-and-checksum defrag-fn block-map)
  (let ((res (defrag-fn block-map)))

    (apply + (map (λ (idx v)
                    (if (not (eq? #\. v))
                        (* idx v)
                        0))
                  (range 0 (length res)) res))))

(define (solve-p1 inp)
  (defragment-and-checksum defragment (parse-and-extract inp)))

(define (solve-p2 inp)
  (defragment-and-checksum defragment-full-blocks (parse-and-extract inp)))

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
  (report-and-compare-results 1 (solve-p1 test-input) 1928)
  (report-and-compare-results 2 (solve-p2 test-input) 2858))

(define (run)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 6421128769094)
    (report-and-compare-results 2 (solve-p2 inp-str) 6448168620520)))
