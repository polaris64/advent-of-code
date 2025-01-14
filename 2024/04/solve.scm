(use-modules (ice-9 textual-ports))

(define test-input
  "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
")

(define (parse-input inp)
  (filter (λ (l) (> (length l) 0))
          (map string->list (string-split inp #\newline))))

(define (next-ch ch)
  (cond ((eq? ch #\X) #\M)
        ((eq? ch #\M) #\A)
        ((eq? ch #\A) #\S)
        (else #f)))

(define (check-directions inp row-idx idx start-ch)
  (define (get ri i)
    (if (or (< ri 0)
            (< i 0)
            (>= ri (length inp))
            (>= i (length (car inp))))
        #f
        (list-ref (list-ref inp ri) i)))

  (define (make-check-dir idx1 idx2 get inc1 inc2)
    (define (check-dir i1 i2 ch a-coord)
      (cond ((not ch) (list #t a-coord))
            ((eq? ch (get i1 i2)) (check-dir (inc1 i1)
                                             (inc2 i2)
                                             (next-ch ch)
                                             (if (eq? ch #\A)
                                                 (cons i1 i2)
                                                 a-coord)))
            (else (list #f #f))))
    (λ () (check-dir idx1 idx2 start-ch #f)))

  (define checks (list
                  (cons 'r (make-check-dir row-idx idx (λ (i j) (get row-idx j)) identity 1+))
                  (cons 'l (make-check-dir row-idx idx (λ (i j) (get row-idx j)) identity 1-))
                  (cons 'd (make-check-dir row-idx idx (λ (i j) (get i idx)) 1+ identity))
                  (cons 'u (make-check-dir row-idx idx (λ (i j) (get i idx)) 1- identity))
                  (cons 'dr (make-check-dir row-idx idx get 1+ 1+))
                  (cons 'ur (make-check-dir row-idx idx get 1- 1+))
                  (cons 'dl (make-check-dir row-idx idx get 1+ 1-))
                  (cons 'ul (make-check-dir row-idx idx get 1- 1-))))


  (let ((res (map (λ (f) (cons (car f) ((cdr f)))) checks)))
    res))

(define (search-words inp start-ch)
  (define (check-row row-idx row)
    (define (iter idx)
      (if (>= idx (length row))
          '()
          (append (check-directions inp row-idx idx start-ch)
                  (iter (1+ idx)))))
    (iter 0))

  (define (check-inp)
    (define (iter row-idx)
      (if (>= row-idx (length inp))
          '()
          (append (check-row row-idx (list-ref inp row-idx))
                  (iter (1+ row-idx)))))
    (iter 0))

  (check-inp))

(define (solve-p1 inp)
  (length (filter (λ (v) (cadr v))
                  (search-words (parse-input inp) #\X))))

(define (solve-p2 inp)
  (let ((ht (make-hash-table)))
    (for-each (λ (v)
                (hash-set! ht
                           (caddr v)
                           (1+ (hash-ref ht (caddr v) 0))))
              (filter (λ (v)
                        (and (cadr v)
                             (memq (car v) (list 'dr 'ur 'dl 'ul))))
                      (search-words (parse-input inp) #\M)))
    (length (filter (λ (v)
                      (= 2 (cdr v)))
                    (hash-map->list cons ht)))))

(solve-p1 test-input)
(solve-p2 test-input)

(call-with-input-file "input.txt"
  (lambda (port)
    ;; p1: 2567
    ;; p2: 2029
    (let ((inp-str (get-string-all port)))
      (format (current-output-port)
              "The solution to part 1 is: ~a~%"
              (solve-p1 inp-str))
      (format (current-output-port)
              "The solution to part 2 is: ~a~%"
              (solve-p2 inp-str)))))
