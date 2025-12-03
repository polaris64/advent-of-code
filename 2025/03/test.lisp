(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/03/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/03))

(in-package :p64/aoc/2025/03/tests)

(def-suite aoc-2025-03 :description "AoC 2025/03")
(in-suite aoc-2025-03)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 357)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 17034))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 3121910778619)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 168798209663590))))

(run! 'aoc-2025-03)
