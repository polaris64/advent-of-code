(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
")

(define (parse-input inp-str)
  (let ((lines (string-split inp-str #\newline)))
    (map (λ (l)
           (map string->number
                (string-split l #\space)))
         (filter (λ (l) (> (string-length l) 0)) lines))))

(define (is-safe report)
  (define (check-order)
    (or
     (equal? report (sort report <))
     (equal? report (reverse (sort report <)))))
  (define (check-diffs curr rest)
    (if (null? rest)
        #t
        (let ((diff (abs (- curr (car rest)))))
          (if (and (>= diff 1) (<= diff 3))
              (and #t (check-diffs (car rest) (cdr rest)))
              #f))))
  (and 
   (check-order)
   (check-diffs (car report) (cdr report))))

(define (remove-at-idx l idx)
  (if (or (< idx 0) (>= idx (length l)))
      l
      (append (take l idx) (drop l (1+ idx)))))

(define (is-safe-dampened report)
  (define (check-with-dampening idx)
    (cond ((is-safe (remove-at-idx report idx)) #t)
          ((>= idx (length report)) #f)
          (else (check-with-dampening (1+ idx)))))
  (if (is-safe report)
      #t
      (check-with-dampening 0)))

(define (solve-p1 inp)
  (length (filter is-safe (parse-input inp))))

(define (solve-p2 inp)
  (length (filter is-safe-dampened (parse-input inp))))

(solve-p1 test-input)
(solve-p2 test-input)

(call-with-input-file "./input.txt"
  (λ (port)
    (let ((inp (get-string-all port)))
      (format (current-output-port) "The solution to part 1 is ~a~%"
              (solve-p1 inp))
      (format (current-output-port) "The solution to part 2 is ~a~%"
              (solve-p2 inp))
      )))
