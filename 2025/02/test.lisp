(ql:quickload "fiveam")

(defpackage :p64/aoc/2025/02/tests
  (:use
   :cl
   :fiveam
   :p64/aoc/2025/02))

(in-package :p64/aoc/2025/02/tests)

(def-suite aoc-2025-02 :description "AoC 2025/02")
(in-suite aoc-2025-02)

(test part-1
  "Test solutions to part 1"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p1 inp) 1227775554)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p1 inp) 31210613313))))

(test part-2
  "Test solutions to part 2"
  (let ((inp (read-input "input_ex.txt")))
    (is (= (solve-p2 inp) 4174379265)))
  (let ((inp (read-input "input.txt")))
    (is (= (solve-p2 inp) 41823587546))))

(run! 'aoc-2025-02)
