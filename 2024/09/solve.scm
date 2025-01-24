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
                            (if (car a) (append (cadr a) (list v)) (cadr a))
                            (if (not (car a)) (append (cddr a) (list v)) (cddr a)))))
                   (cons #t (cons (list) (list))) 
                   (map (compose string->number string)
                        (filter (λ (ch) (not (eq? #\newline ch)))
                                (string->list inp-str))))))
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
  (list->array 1 (iter (list) file-blocks free-blocks 0)))

(define (parse-and-extract inp-str)
  (call-with-values (λ () (parse-input inp-str))
    (λ (file-blocks free-blocks)
      (extract-block-map file-blocks free-blocks))))

(define (empty-block? block)
  (eq? block #\.))

(define (defragmented? block-map)
  (let ((idx-start (get-first-empty-block-idx block-map)))
    (define (iter idx)
      (cond ((not (empty-block? (array-ref block-map idx))) #f)
            ((= idx (1- (array-length block-map))) #t)
            (else (iter (1+ idx)))))
    (iter idx-start)))

(define (defragment block-map)
  (if (defragmented? block-map)
      (array->list block-map)
      (let* ((i1 (get-first-empty-block-idx block-map))
             (i2 (get-last-non-empty-block-idx block-map))
             (v (array-ref block-map i1)))
        (array-set! block-map (array-ref block-map i2) i1)
        (array-set! block-map v i2)
        (defragment block-map))))

(define (get-first-empty-block-idx block-map)
  (define (iter idx)
    (cond ((empty-block? (array-ref block-map idx)) idx)
          ((>= idx (array-length block-map)) #f)
          (else (iter (1+ idx)))))
  (iter 0))

(define (get-last-non-empty-block-idx block-map)
  (define (iter idx)
    (cond ((not (empty-block? (array-ref block-map idx))) idx)
          ((= idx 0) #f)
          (else (iter (1- idx)))))
  (iter (1- (array-length block-map))))

(define (solve-p1 inp)
  (let ((res (filter (compose not empty-block?)
                     (defragment
                       (parse-and-extract inp)))))
    (apply + (map (λ (idx v) (* idx v)) (range 0 (length res)) res))))

(define (solve-p2 inp)
  #f)

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
  (report-and-compare-results 2 (solve-p2 test-input) #f))

(define (run)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 6421128769094)
    (report-and-compare-results 2 (solve-p2 inp-str) #f)))
