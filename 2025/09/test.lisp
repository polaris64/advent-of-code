(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/09/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/09))

(in-package :p64/aoc/2025/09/tests)

(def-suite aoc-2025-09 :description "AoC 2025/09")
(in-suite aoc-2025-09)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 50)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 4741451444))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 1)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 1))))

(run! 'aoc-2025-09)
