#!/usr/bin/guile \
-e main -s

!#
(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input
  "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (get-string-all port))))

(define (parse-input inp-str)
  (map (λ (line)
         (let* ((parts (string-split line #\:))
                (test-value (string->number (car parts)))
                (numbers (filter identity (map string->number (string-split (cadr parts) #\space)))))
           (cons test-value numbers)))
       (filter (λ (line) (> (string-length line) 0))
               (string-split inp-str #\newline))))

(define (eval-expr-part acc part)
  (cond ((eq? (car part) '+) (+ acc (cdr part)))
        ((eq? (car part) '*) (* acc (cdr part)))
        ((eq? (car part) '||) (string->number (string-concatenate (list (number->string acc) (number->string (cdr part))))))
        (else (error "EVAL-EXPR-PART: unknown operator" (car part)))))

(define (eval-expr expr)
  (define (iter res rest-expr)
    (if (null? rest-expr)
        res
        (iter
         (eval-expr-part res (car rest-expr))
         (cdr rest-expr))))
  (iter (car expr) (cdr expr)))

(define (build-expr numbers operators)
  (cons (car numbers)
        (map (λ (n o) (cons o n)) (cdr numbers) operators)))

(define (combinations values len)
  (if (= len 0)
      (list '())
      (append-map (λ (op)
                    (map (λ (op2) (cons op op2))
                         (combinations values (1- len))))
                  values)))

(define (try-combinations test-val numbers op-symbols)
  (any (λ (operators)
         (eq? test-val (eval-expr (build-expr numbers operators))))
       (combinations op-symbols (1- (length numbers)))))

(define (sum-of-possible-equations inp op-symbols)
  (apply +
         (map car (filter (λ (v) (cdr v))
                          (map (λ (v)
                                 (cons (car v) (try-combinations (car v) (cdr v) op-symbols)))
                               (parse-input inp))))))

(define (solve-p1 inp)
  (sum-of-possible-equations inp (list '+ '*)))

(define (solve-p2 inp)
  (sum-of-possible-equations inp (list '+ '* '||)))

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
  (report-and-compare-results 1 (solve-p1 test-input) 3749)
  (report-and-compare-results 2 (solve-p2 test-input) 11387))

(define (main args)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 538191549061)
    (report-and-compare-results 2 (solve-p2 inp-str) 34612812972206)))
