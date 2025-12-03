(ql:quickload "alexandria")
(ql:quickload "serapeum")

(defpackage :p64/aoc/2025/02
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/02)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (parse-ranges 
     (car
      (loop :for line := (read-line s nil nil)
            :while line
            :collect (uiop:split-string line :separator '(#\,)))))))

(defun parse-ranges (groups)
  (mapcar (lambda (group)
            (let ((parts (uiop:split-string group :separator '(#\-))))
              (cons (parse-integer (car parts)) (parse-integer (cadr parts)))))
          groups))

(defun invalid-id-p (x)
  (let ((x-str (coerce (write-to-string x :base 10) 'list)))
    (equal (subseq x-str 0 (floor (/ (list-length x-str) 2)))
           (subseq x-str (floor (/ (list-length x-str) 2))))))

(defun id-is-repeated-sequence (seq id)
  (let ((id-seq (coerce (write-to-string id :base 10) 'list)))
    (every (lambda (subseq) (equal seq subseq))
          (serapeum:batches id-seq (list-length seq)))))

(defun invalid-id-p2-p (id)
  (let ((id-str (coerce (write-to-string id :base 10) 'list)))
    (loop :for x :from 1 :to (floor (/ (list-length id-str) 2))
          :when (id-is-repeated-sequence (subseq id-str 0 x) id)
            :return t)))

(defun get-invalid-ids-in-range (range &optional test-fn)
  (loop :for x :from (car range) :to (cdr range)
        :when (funcall (or test-fn #'invalid-id-p) x)
          :collect x))

(defun solve-p1 (input)
  (apply #'+
         (alexandria:flatten
          (mapcar #'get-invalid-ids-in-range input))))

(defun solve-p2 (input)
  (apply #'+
         (alexandria:flatten
          (mapcar (lambda (v) (get-invalid-ids-in-range v #'invalid-id-p2-p)) input))))

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)
