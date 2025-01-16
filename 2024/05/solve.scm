(use-modules (ice-9 regex)
             (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input 
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
")

(define (parse-input inp-str)
  (let* ((lines (string-split inp-str #\newline))
         (sep-idx (list-index (λ (s) (string-match "^$" s)) lines)))
    (values (map (λ (l) (map string->number (string-split l #\|)))
                 (take lines sep-idx))
            (map (λ (l) (map string->number (string-split l #\,)))
                 (filter (λ (l) (> (string-length l) 0))
                         (drop lines (1+ sep-idx)))))))

(define (check-rule-for-update rule update)
  (cond ((and (memq (car rule) update) (memq (cadr rule) update))
         (let ((i1 (list-index (λ (x) (= (car rule) x)) update))
               (i2 (list-index (λ (x) (= (cadr rule) x)) update)))
           (> i2 i1)))
        (else #t)))

(define (validate-update update rules)
  (let* ((first-invalid (drop-while (λ (r) (check-rule-for-update r update))
                                    rules))
         (invalid-len (length first-invalid)))
    (values
     (= 0 invalid-len)
     (if (= 0 invalid-len)
         '()
         (car first-invalid)))))

(define (is-update-valid? update rules)
  (call-with-values (λ () (validate-update update rules)) (λ (valid? _) valid?)))

(define (get-valid-updates updates rules)
  (filter (λ (update) (is-update-valid? update rules)) updates))

(define (get-invalid-updates updates rules)
  (filter (λ (update) (not (is-update-valid? update rules))) updates))

(define (get-middle-page update)
  (list-ref update (floor (/ (length update) 2))))

(define (flip-pair! update i1 i2)
  (let ((v1 (list-ref update i1))
        (v2 (list-ref update i2)))
    (list-set! update i1 v2)
    (list-set! update i2 v1)
    update))

(define (resolve-update update rules)
  (define (iter)
    (call-with-values (λ () (validate-update update rules))
      (λ (valid? matching-rule)
        (if valid?
            update
            (begin
              (let ((i1 (list-index (λ (v) (= v (car matching-rule))) update))
                    (i2 (list-index (λ (v) (= v (cadr matching-rule))) update)))
                (flip-pair! update i1 i2)
                (iter)))))))
  (iter))

(define (solve-p1 inp)
  (call-with-values (λ () (parse-input inp))
    (λ (rules updates)
      (apply + (map get-middle-page (get-valid-updates updates rules))))))

(define (solve-p2 inp)
  (call-with-values (λ () (parse-input inp))
    (λ (rules updates)
      (apply + (map get-middle-page
                    (map (λ (update)
                           (resolve-update update rules))
                         (get-invalid-updates updates rules)))))))

(solve-p1 test-input)
(solve-p2 test-input)

(call-with-input-file "input.txt"
  (lambda (port)
    (let* ((inp-str (get-string-all port))
           (p1 (solve-p1 inp-str))
           (p2 (solve-p2 inp-str)))
      (format (current-output-port) "The solution to part 1 is: ~a~%" p1)
      (if (not (= 5509 p1))
          (error "Part 1 solution is incorrect: 5509 !=" p1))
      (format (current-output-port) "The solution to part 2 is: ~a~%" p2)
      (if (not (= 4407 p2))
          (error "Part 2 solution is incorrect: 4407 !=" p2)))))
