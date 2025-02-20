#lang racket

(module+ test
  (require rackunit))

(define test-input "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (parse-input inp-str)
  (let* ((lines (string-split inp-str "\n"))
         (split-at (index-where lines (λ (l) (= 0 (string-length l))))))

    (values (map string->list (string-split (car (take lines split-at)) ", "))
            (map string->list (drop lines (+ 1 split-at))))))

(define (is-design-possible design patterns)
  (define cache (make-hash))
  (define (iter design)
    (if (empty? design)
        #t
        (let ((cached (hash-ref cache design 'UNKNOWN)))
          (if (not (eq? 'UNKNOWN cached))
              cached
              (let ((res (findf (λ (pattern)
                                  (if (and (>= (length design) (length pattern))
                                           (equal? (take design (length pattern)) pattern))
                                      (iter (drop design (length pattern)))
                                      #f))
                                patterns)))
                (hash-set! cache design res)
                (if res #t #f))))))
  (iter design))

(define (get-design-combinations design patterns)
  (define cache (make-hash))
  (define (iter design)
    (if (empty? design)
        1
        (let ((cached (hash-ref cache design 'UNKNOWN)))
          (if (not (eq? 'UNKNOWN cached))
              cached
              (let ((res (apply + (map (λ (pattern)
                                         (if (and (>= (length design) (length pattern))
                                                  (equal? (take design (length pattern)) pattern))
                                             (iter (drop design (length pattern)))
                                             0))
                                       patterns))))
                (hash-set! cache design res)
                res)))))
  (iter design))

(define (solve-p1 inp)
  (let-values (((patterns designs) (parse-input inp)))
    (length (filter (λ (design) (is-design-possible design patterns)) designs))))

(define (solve-p2 inp)
  (let-values (((patterns designs) (parse-input inp)))
    (apply + (map (λ (design) (get-design-combinations design patterns)) designs))))

(module+ test
  (check-eq? (solve-p1 test-input) 6)
  (check-eq? (solve-p2 test-input) 16)
  (check-eq? (solve-p1 (read-input)) 350)
  (check-eq? (solve-p2 (read-input)) 769668867512623))

(define (report-result part result)
  (printf "The solution to part ~a is: ~a~%" part result))

(define (run-tests)
  (report-result 1 (solve-p1 test-input))
  (report-result 2 (solve-p2 test-input)))

(define (run)
  (let ((inp (read-input)))
    (report-result 1 (solve-p1 inp))
    (report-result 2 (solve-p2 inp))))

(run)
