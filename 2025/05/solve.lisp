(defpackage :p64/aoc/2025/05
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/05)

(defun parse-ingredient (v)
  (parse-integer v))

(defun parse-range (v)
  (let ((start-end (mapcar #'parse-integer (uiop:split-string v :separator '(#\-)))))
    (cons (car start-end) (cadr start-end))))

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (let ((ranges (loop :for line := (read-line s nil nil)
                        :while (and line (> (length line) 0))
                        :collect (parse-range line)))
          (ingredients (loop :for line := (read-line s nil nil)
                             :while line
                             :collect (parse-ingredient line))))
      (values ranges ingredients))))

(defun is-in-range (i range)
  (and (<= (car range) i) (>= (cdr range) i)))

(defun is-in-any-range (i ranges)
  (some (lambda (r) (is-in-range i r)) ranges))

(defun range-overlaps-p (r1 r2)
  (let ((r1s (car r1))
        (r1e (cdr r1))
        (r2s (car r2))
        (r2e (cdr r2)))
    (or
     (and (<= r1s r2s) (>= r1e r2s))
     (and (<= r1s r2e) (>= r1e r2e)))))

(defun combine-ranges (r1 r2)
  (cons (min (car r1) (car r2))
        (max (cdr r1) (cdr r2))))

(defun get-overlapping-range (ranges range)
  (loop for r in ranges
        when (range-overlaps-p range r)
          return r))

(defun combine-all-ranges (ranges)
  (let ((new-ranges (list (car ranges))))
    (loop for r in (cdr ranges)
          collect (let ((overlapping-range (get-overlapping-range new-ranges r)))
                    (cond (overlapping-range (let ((combined (combine-ranges overlapping-range r)))
                                               (setf (car overlapping-range) (car combined))
                                               (setf (cdr overlapping-range) (cdr combined))))
                          (t (setf new-ranges (cons r new-ranges))))))
    new-ranges))

(defun combine-all-ranges-recursive (ranges)
  (let ((prev nil)
        (new ranges))
    (loop while (set-exclusive-or new prev :test #'equal)
          do (let ((combined (combine-all-ranges new)))
               (setf prev new)
               (setf new combined)))
    new))

(defun count-range (range)
  (1+ (- (cdr range) (car range))))

(defun count-all-ranges (ranges)
  (reduce (lambda (a r) (+ a (count-range r)))
          ranges
          :initial-value 0))

(defun solve-p1 (ranges ingredients)
  (loop for i in ingredients
        when (is-in-any-range i ranges)
          count 1))

(defun solve-p2 (ranges)
  (count-all-ranges (combine-all-ranges-recursive ranges)))

(defun run ()
  (multiple-value-bind (ranges ingredients) (read-input "input.txt")
    (format t "The solution to part 1 is: ~a~%" (solve-p1 ranges ingredients))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 ranges))))

(run)
