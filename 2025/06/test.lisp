(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/06/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/06))

(in-package :p64/aoc/2025/06/tests)

(def-suite aoc-2025-06 :description "AoC 2025/06")
(in-suite aoc-2025-06)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 4277556)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 6757749566978))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input-p2 "input_ex.txt")))
    (is (= (solve-p2 inp) 3263827)))
  (let ((inp (read-input-p2 "input.txt")))
    (is (= (solve-p2 inp) 10603075273949))))

(run! 'aoc-2025-06)
