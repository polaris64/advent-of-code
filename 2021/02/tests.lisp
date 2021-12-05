(defpackage :aoc-2021-01-tests
  (:use :cl :lisp-unit :aoc-2021-01))

(in-package :aoc-2021-01-tests)

(setq *print-failures* t)

(define-test test-solve-p1-example
  (assert-equal 7 (solve-p1 (read-input "input_ex.txt"))))

(define-test test-solve-p1
  (assert-equal 1502 (solve-p1 (read-input "input.txt"))))

(define-test test-solve-p2-example
  (assert-equal 5 (solve-p2 (read-input "input_ex.txt"))))

(define-test test-solve-p2
  (assert-equal 1538 (solve-p2 (read-input "input.txt"))))

(run-tests :all)
