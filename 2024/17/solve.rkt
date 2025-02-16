(define test-input "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0
")

(define (parse-input inp-str)
  (let ((registers (make-hash))
        (program (list)))
    (map (λ (line)
           (let ((match-reg (regexp-match #rx"Register ([A-C]): ([0-9]+)"
                                          line))
                 (match-prog (regexp-match #rx"Program: (([0-9]+,?)+)"
                                           line)))
             (when match-reg
               (hash-set! registers (string->symbol (cadr match-reg)) (string->number (caddr match-reg))))
             (when match-prog
               (set! program (list->vector (map string->number (string-split (cadr match-prog) ",")))))))
         (string-split inp-str "\n"))
    (values registers
            program)))

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (get-combo-operand-value registers operand)
  (cond ((and (>= operand 0) (<= operand 3)) operand)
        ((= operand 4) (hash-ref registers 'A))
        ((= operand 5) (hash-ref registers 'B))
        ((= operand 6) (hash-ref registers 'C))
        (else (error 'get-combo-operand-value (format "Unknown combo operand: ~a" operand)))))

(define (process-instruction ip registers opcode operand)
  (let ((output (list))
        (new-ip (+ 2 ip)))

    (cond

     ;; ADV
     ((= opcode 0)
      (hash-set! registers 'A
                 (truncate (/ (hash-ref registers 'A)
                              (expt 2 (get-combo-operand-value registers operand)))))) 

     ;; BXL
     ((= opcode 1)
      (hash-set! registers
                 'B
                 (bitwise-xor 
                  (hash-ref registers 'B)
                  operand)))

     ;; BST
     ((= opcode 2)
      (hash-set! registers
                 'B
                 (modulo (get-combo-operand-value registers operand) 8)))

     ;; JNZ
     ((= opcode 3)
      (when (not (= 0 (hash-ref registers 'A)))
        (set! new-ip operand)))

     ;; BXC
     ((= opcode 4)
      (hash-set! registers
                 'B
                 (bitwise-xor 
                  (hash-ref registers 'B)
                  (hash-ref registers 'C))))

     ;; OUT
     ((= opcode 5)
      (set! output (list (modulo (get-combo-operand-value registers operand) 8))))

     ;; BDV
     ((= opcode 6)
      (hash-set! registers 'B
                 (truncate (/ (hash-ref registers 'A)
                              (expt 2 (get-combo-operand-value registers operand))))))

     ;; CDV
     ((= opcode 7)
      (hash-set! registers 'C
                 (truncate (/ (hash-ref registers 'A)
                              (expt 2 (get-combo-operand-value registers operand))))))

     (else (error 'process-instruction (format "Unknown opcode: ~a" opcode))))

    (values output new-ip)))

(define (run-program registers program ip)
  (define (iter output ip)
    (if (>= ip (- (vector-length program) 1))
        output
        
        (let-values (((op-output new-ip) (process-instruction ip
                                                              registers
                                                              (vector-ref program ip)
                                                              (vector-ref program (+ 1 ip)))))
          ;; (printf "ip=~a, registers=~a, op-output=~a, new-ip=~a\n"
          ;;         ip
          ;;         registers
          ;;         op-output
          ;;         new-ip)
          (iter (append output op-output) new-ip))))
  (iter (list) ip))

(define (solve-p1 inp)
  (let-values (((registers program) (parse-input inp)))
    (string-join (map number->string (run-program registers program 0)) ",")))

(define (solve-p2 inp)
  #f)

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (display
   (format "The solution to part ~a is: ~a~%" part result)))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input) "4,6,3,5,6,3,5,2,1,0")
  (report-and-compare-results 2 (solve-p2 test-input) #f))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp) "7,4,2,0,5,0,5,3,7")
    (report-and-compare-results 2 (solve-p2 inp) #f)))
