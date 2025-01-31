(use-modules (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1))

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
      (get-string-all port))))

(define (parse-input inp-str)
  (define (get-sections lines)
    (if (null? lines)
        '()
        (call-with-values
            (λ () (span (λ (line)
                          (> (string-length line) 0))
                        lines))
          (λ (chunk rest)
            (if (null? rest)
                (list chunk)
                (append (list chunk) (get-sections (cdr rest))))))))

  (define (extract-coords pat line)
    (let ((m (string-match pat line)))
      (let ((coords (string-split (string-drop line (match:end m))
                                  #\,)))
        (map (λ (v) (string->number (match:substring (string-match "[0-9]+" v))))
             coords))))

  (define (parse-section section)
    (map (λ (line)
           (cond ((string-match "^Button A:" line) (extract-coords "^Button A:" line))
                 ((string-match "^Button B:" line) (extract-coords "^Button B:" line))
                 ((string-match "^Prize:" line) (extract-coords "^Prize: " line))))
         section))

  (map parse-section
       (get-sections
        (string-split inp-str #\newline))))

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
  (format (current-output-port) "The solution to part ~a is: ~a~%" part result))

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
