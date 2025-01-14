(use-modules (ice-9 regex)
             (ice-9 textual-ports))

(define test-input-p1 "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))")
(define test-input-p2 "2xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))")

(define (get-valid-instructions inp)
  (map match:substring
       (list-matches "mul\\([0-9]{1,3},[0-9]{1,3}\\)" inp)))

(define (get-valid-instructions-p2 inp)
  (map match:substring
       (list-matches "(mul\\([0-9]{1,3},[0-9]{1,3}\\)|do\\(\\)|don't\\(\\))" inp)))

(define (run-instruction instr)
  (let ((parts (map (compose string->number match:substring)
                    (list-matches "[0-9]{1,3}" instr))))
    (apply * parts)))

(define (run-instructions-p2 instrs)
  (define (iter mul-enabled? rest)
    (cond ((null? rest) 0)
          ((equal? "don't()" (car rest)) (iter #f (cdr rest)))
          ((equal? "do()" (car rest)) (iter #t (cdr rest)))
          (else (+ (if mul-enabled?
                       (run-instruction (car rest))
                       0)
                   (iter mul-enabled? (cdr rest))))))
  (iter #t instrs))

(define (solve-p1 inp)
  (apply + (map run-instruction (get-valid-instructions inp))))

(define (solve-p2 inp)
  (run-instructions-p2 (get-valid-instructions-p2 inp)))

(solve-p2 test-input-p2)

(call-with-input-file "input.txt"
  (lambda (port)
    ;; p1: 174561379
    ;; p2: 106921067
    (let ((inp-str (get-string-all port)))
      (format (current-output-port)
              "The solution to part 1 is: ~a~%"
              (solve-p1 inp-str))
      (format (current-output-port)
              "The solution to part 2 is: ~a~%"
              (solve-p2 inp-str)))))
