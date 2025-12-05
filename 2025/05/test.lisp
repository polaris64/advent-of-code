(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/05/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/05))

(in-package :p64/aoc/2025/05/tests)

(def-suite aoc-2025-05 :description "AoC 2025/05")
(in-suite aoc-2025-05)

(test part-1
  "Test solutions to part 1"
  (multiple-value-bind (ranges ingredients) (read-input "input_ex.txt")
    (is (= (solve-p1 ranges ingredients) 3)))
  (multiple-value-bind (ranges ingredients) (read-input "input.txt")
    (is (= (solve-p1 ranges ingredients) 698))))

(test part-2
  "Test solutions to part 2"
  (multiple-value-bind (ranges ingredients) (read-input "input_ex.txt")
    (is (= (solve-p2 ranges) 14)))
  (multiple-value-bind (ranges ingredients) (read-input "input.txt")
    (is (= (solve-p2 ranges) 352807801032167))))

(run! 'aoc-2025-05)
