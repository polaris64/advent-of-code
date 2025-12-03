(ql:quickload "alexandria")

(defpackage :p64/aoc/2025/03
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/03)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (mapcar (lambda (line)
              (map 'list (lambda (x) (parse-integer (string x))) line))
            (alexandria:flatten
             (loop :for line := (read-line s nil nil)
                   :while line
                   :collect (uiop:split-string line :separator '(#\,)))))))

(defun digits-to-int (digits)
  (parse-integer
   (concatenate 'string (mapcar #'digit-char digits))))

(defun get-digit-groups (l)
  (let ((len (list-length l)))
    (labels ((recurse (i1 i2)
               (cond ((< i2 len) (cons (digits-to-int (list (nth i1 l) (nth i2 l))) (recurse i1 (1+ i2))))
                     ((< i1 len) (recurse (1+ i1) (+ i1 2)))
                     (t nil))))
      (recurse 0 1))))

;; NOTE: This brute-force recursive solution works but is too slow for
;; the main input
#+nil
(defun get-n-digit-groups (bank n)
  (let ((len (list-length bank)))
    (labels ((recurse (group-len i)
               (cond ((= group-len n) (list nil))
                     ((>= i len) nil)
                     (t
                      (append

                       ;; This path can return either (list nil) or nil. For
                       ;; (list nil), mapcar will cons the current digit and
                       ;; nil, ending the list. For nil, mapcar will not be run
                       ;; and an empty list will be used for append.
                       (mapcar (lambda (sub) (cons (nth i bank) sub))
                               (recurse (1+ group-len) (1+ i)))

                       ;; group-len cannot exceed n on this path, so the base
                       ;; case for i exceeding len will return nil which causes
                       ;; append to return its first argument
                       (recurse group-len (1+ i)))))))

      (recurse 0 0))))

(defun largest-n-digit-group (bank n)
  ;; Create a monotonic (decreasing) stack with a maximum length of n
  (let ((stack (make-array n :fill-pointer 0))
        (len (list-length bank)))
    (loop for digit in bank
          for i from 0
          do
             (cond
               ;; Stack is empty or last stack digit is >= the
               ;; current digit: push current digit onto stack
               ((or (= 0 (fill-pointer stack))
                    (>= (aref stack (1- (fill-pointer stack))) digit))
                (vector-push digit stack))

               ;; Otherwise pop the stack while the fill pointer is >
               ;; 0, the last stack digit is < the current digit and
               ;; there are enough remaining digits in the digit bank
               ;; to fill the desired size n
               (t (loop while (and (> (fill-pointer stack) 0)
                                   (< (aref stack (1- (fill-pointer stack))) digit)
                                   (>= (+ (1- (fill-pointer stack)) (- len i)) n))
                        do (decf (fill-pointer stack)))
                  (when (< (fill-pointer stack) n)
                    (vector-push digit stack)))))
    
    (coerce stack 'list)))

(defun get-max-group (digit-groups)
  (apply #'max digit-groups))

(defun solve-p1 (input)
  (apply #'+ (mapcar (lambda (bank)
                       (get-max-group (get-digit-groups bank)))
                     input)))

(defun solve-p2 (input)
  (apply #'+
         (mapcar (lambda (bank) (digits-to-int (largest-n-digit-group bank 12)))
                 input)))

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)
