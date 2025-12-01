(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/01/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/01))

(in-package :p64/aoc/2025/01/tests)

(def-suite aoc-2025-01 :description "AoC 2025/01")
(in-suite aoc-2025-01)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 3)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 1074))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 6)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 6254))))

(run! 'aoc-2025-01)
