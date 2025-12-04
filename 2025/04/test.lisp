(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/04/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/04))

(in-package :p64/aoc/2025/04/tests)

(def-suite aoc-2025-04 :description "AoC 2025/04")
(in-suite aoc-2025-04)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 13)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 1320))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 43)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 8354))))

(run! 'aoc-2025-04)
