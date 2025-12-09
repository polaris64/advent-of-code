(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/08/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/08))

(in-package :p64/aoc/2025/08/tests)

(def-suite aoc-2025-08 :description "AoC 2025/08")
(in-suite aoc-2025-08)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp 3 10) 40)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp 3 1000) 66640))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 25272)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 78894156))))

(run! 'aoc-2025-08)
