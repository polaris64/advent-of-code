(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/07/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/07))

(in-package :p64/aoc/2025/07/tests)

(def-suite aoc-2025-07 :description "AoC 2025/07")
(in-suite aoc-2025-07)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 21)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 1594))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 40)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 15650261281478))))

(run! 'aoc-2025-07)
