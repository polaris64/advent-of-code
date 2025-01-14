(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input "3   4
4   3
2   5
1   3
3   9
3   3")

(define (parse-input inp)
  (call-with-input-string inp
    (λ (port)
      (define (r)
        (let ((n1 (read port))
              (n2 (read port)))
          (if (or (eof-object? n1) (eof-object? n2))
              '()
              (cons (list n1 n2) (r)))))
      (r))))

(define (count-occurrences n lst)
  (if (or (null? lst) (> (car lst) n))
      0
      (+ (if (= n (car lst)) 1 0) (count-occurrences n (cdr lst)))))

(define (get-input-lists inp-str)
  (let* ((lists (parse-input inp-str))
         (l1 (map car lists))
         (l2 (map cadr lists)))
    (values l1 l2)))

(define (solve-p1 inp)
  (call-with-values (λ () (get-input-lists inp))
    (λ (l1 l2)
      (apply + (map (compose abs -)
                    (sort l1 <)
                    (sort l2 <))))))

(define (solve-p2 inp)
  (call-with-values (λ () (get-input-lists inp))
    (λ (l1 l2)
      (let ((l2 (sort l2 <)))
        (apply + (map (λ (n)
                        (* n (count-occurrences n l2)))
                      l1))))))

(call-with-input-file "./input.txt"
  (λ (port)
    (let ((inp (get-string-all port)))
      (format (current-output-port) "The solution to part 1 is ~a~%"
              (solve-p1 inp))
      (format (current-output-port) "The solution to part 2 is ~a~%"
              (solve-p2 inp)))))
