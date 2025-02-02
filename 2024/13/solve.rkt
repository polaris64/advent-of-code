#lang racket

(require plot)

(define test-input "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (parse-input inp-str)
  (define (get-sections lines)
    (if (null? lines)
        '()
        (let ((chunk (takef lines (λ (line) (> (string-length line) 0))))
              (rest (dropf lines (λ (line) (> (string-length line) 0)))))
          (if (null? rest)
              (list chunk)
              (append (list chunk) (get-sections (cdr rest)))))))

  (define (parse-section section)
    (map (λ (line)
           (cond ((regexp-match "^Button A:" line)
                  (let ((m (regexp-match "^Button A: X\\+([0-9]+), Y\\+([0-9]+)" line)))
                    (map string->number (list (list-ref m 1)
                                              (list-ref m 2)))))
                 ((regexp-match "^Button B:" line)
                  (let ((m (regexp-match "^Button B: X\\+([0-9]+), Y\\+([0-9]+)" line)))
                    (map string->number (list (list-ref m 1)
                                              (list-ref m 2)))))
                 ((regexp-match "^Prize:" line)
                  (let ((m (regexp-match "^Prize: X=([0-9]+), Y=([0-9]+)" line)))
                    (map string->number (list (list-ref m 1)
                                              (list-ref m 2)))))))
         section))

  (map parse-section
       (get-sections
        (string-split inp-str "\n"))))

(define (expr-x expr)
  (car expr))

(define (expr-y expr)
  (cadr expr))

(define (solve button-a button-b prize)
  (let* ((x-coef (+ 
                  (expr-y button-a)
                  (- (* (expr-y button-b)
                        (/ (expr-x button-a) (expr-x button-b))))))
         (const (- (expr-y prize)
                   (* (expr-y button-b)
                      (/ (expr-x prize)
                         (expr-x button-b)))))
         (x (/ const x-coef))
         (y (/ (- (expr-x prize)
                  (* (expr-x button-a) x))
               (expr-x button-b))))
    (cons (exact->inexact x)
          (exact->inexact y))))

(define (visualise-solution button-a button-b prize)
  (let ((solution (solve button-a button-b prize)))
    (plot (list
           (axes)
           (function (λ (x) (/ (- (car prize) (* (car button-a) x)) (car button-b)))
                     (- (car solution) 50)
                     (+ (car solution) 50))
           (function (λ (x) (/ (- (cadr prize) (* (cadr button-a) x)) (cadr button-b)))
                     (- (car solution) 50)
                     (+ (car solution) 50))
           (point-label (vector (car solution) (cdr solution)))))))

;; (apply visualise-solution (list-ref (parse-input (read-input)) 0))

(define (has-valid-solution? solution)
  (and
   (integer? (car solution))
   (integer? (cdr solution))))

(define (get-solution-cost solution)
  (inexact->exact (+ (* (car solution) 3)
                     (cdr solution))))

(define (calculate-total-costs groups)
  (apply +
         (map get-solution-cost
              (filter has-valid-solution?
                      (map (λ (t)
                             (solve (car t) (cadr t) (caddr t)))
                           groups)))))

(define (solve-p1 inp)
  (calculate-total-costs (parse-input inp)))

(define (modify-prize-positions inp)
  (map (λ (group)
         (list (car group)
               (cadr group)
               (map (λ (x) (+ x 10000000000000))
                    (caddr group)))
         )
       inp))

(define (solve-p2 inp)
  (calculate-total-costs (modify-prize-positions (parse-input inp))))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format #f "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (display
   (format "The solution to part ~a is: ~a~%" part result)))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input) 480)
  (report-and-compare-results 2 (solve-p2 test-input) 875318608908))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp) 31065)
    (report-and-compare-results 2 (solve-p2 inp) 93866170395343)))
