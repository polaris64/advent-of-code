(defpackage :aoc-2021-01
  (:use :cl)
  (:export read-input solve-p1 solve-p2))

(in-package :aoc-2021-01)

(defun read-input (filename)
  (with-open-file (in filename)
    (loop for line = (read-line in nil nil)
          while line
          collecting (parse-integer line))))

(defun solve-p1 (input)
  (reduce #'+ (loop for x in input
                    for y in (cdr input)
                    collecting (if (> y x) 1 0))))

(defun solve-p2 (input)
  (let ((windows (loop for x in input
                       for y in (cdr input)
                       for z in (cddr input)
                       collecting (+ x y z))))
     (solve-p1 windows)))

(defun main ()
  (format t "The solution to part 1 is: ~a~%" (solve-p1 (read-input "input.txt")))
  (format t "The solution to part 2 is: ~a~%" (solve-p2 (read-input "input.txt"))))

(main)
