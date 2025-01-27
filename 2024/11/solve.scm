(use-modules (ice-9 textual-ports)
             (srfi srfi-1))

(define test-input "125 17\n")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (get-string-all port))))

(define (parse-input inp-str)
  (map (λ (s) (string->number (string-filter (λ (c) (not (eq? #\newline c))) s)))
       (string-split inp-str #\space)))

(define (split-number num)
  (let* ((num-str (number->string num))
         (str-len (string-length num-str)))
    (if (not (= 0 (modulo str-len 2)))
        (error "split-number: number does not have an even number of digits:" num)
        (let ((digits (string->list num-str)))
          (list
           (string->number (list->string (list-head digits (/ str-len 2))))
           (string->number (list->string (list-tail digits (/ str-len 2)))))))))

;; Initial solution: build up a list of stones based on the rules for
;; one "blink"
(define (process-stones stones)
  (cond ((null? stones) '())
        ((= 0 (car stones))
         (cons 1
               (process-stones (cdr stones))))
        ((= 0 (modulo (string-length (number->string (car stones))) 2))
         (append (split-number (car stones))
                 (process-stones (cdr stones))))
        (else (cons (* 2024 (car stones))
                    (process-stones (cdr stones))))))

;; Second solution: process each stone at a time and keep track of the
;; total accumulated number over all "blinks". Use memoisation
;; (hash-table) to improve speed.
(define (process-stones-p2 stones blinks)
  (define cache (make-hash-table))
  
  (define (process-stone stone remaining-blinks)

    ;; The stone cannot split if there are no more blinks, so 1 stone
    ;; produces 1 stone
    (if (= remaining-blinks 0)
        1

        ;; Check for cached results
        (let ((cached (hash-ref cache (cons stone remaining-blinks))))
          (if cached
              cached

              ;; Process this stone: -
              ;; - If the stone is 0, result is processing the rest of
              ;;   the blinks starting with its replacement 1
              ;; - If the stone has an even number of digits, the
              ;;   result is the sum of processing two new stones with
              ;;   the corresponding digits for the rest of the blinks.
              ;; - Otherwise the result is processing a replacement
              ;;   stone with a new value of the current value
              ;;   multiplied by 2024.
              (let ((res (cond ((= stone 0) (process-stone 1 (1- remaining-blinks)))
                               ((= 0 (modulo (string-length (number->string stone)) 2))
                                (apply + (map (λ (s) (process-stone s (1- remaining-blinks)))
                                              (split-number stone))))
                               (else (process-stone (* stone 2024) (1- remaining-blinks))))))

                ;; Add to cache and return
                (hash-set! cache (cons stone remaining-blinks) res)
                res)))))

  (map (λ (s) (process-stone s blinks)) stones))

(define (solve-p1 inp)
  (length 
   (fold (λ (_  stones) (process-stones stones)) (parse-input inp) (iota 25))))

(define (solve-p2 inp)
  (apply + (process-stones-p2 (parse-input inp) 75)))

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
  (report-and-compare-results 1 (solve-p1 test-input) 55312)
  (report-and-compare-results 2 (solve-p2 test-input) 65601038650482))

(define (run)
  (let ((inp-str (read-input)))
    (report-and-compare-results 1 (solve-p1 inp-str) 194782)
    (report-and-compare-results 2 (solve-p2 inp-str) 233007586663131)))
