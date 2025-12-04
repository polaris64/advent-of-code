(defpackage :p64/aoc/2025/04
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/04)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (loop :for line := (read-line s nil nil)
          :while line
          :collect (coerce line 'list))))

(defun get-grid (input)
  (let ((grid (make-hash-table :test #'equal)))
    (loop for row in input
          for y from 0
          do (loop for cell in row
                   for x from 0
                   when (eq cell #\@)
                     do (setf (gethash (cons x y) grid) t)))
    grid))

(defun count-neighbours (grid coord)
  (loop for (x-off . y-off) in
                            (loop for x from -1 to 1
                                  append (loop for y from -1 to 1
                                               when (not (and (= x 0) (= y 0)))
                                                 collect (cons x y)))
        count (gethash (cons (+ (car coord) x-off)
                             (+ (cdr coord) y-off))
                       grid)))

(defun get-removable-cells (grid neighbour-count)
  (loop for coord being the hash-key of grid
        when (<= (count-neighbours grid coord) neighbour-count)
          collect coord))

(defun remove-cells! (grid coords)
  (loop for coord in coords
        do (remhash coord grid))
  (list-length coords))

(defun move-all-paper! (grid)
  (let* ((cells-to-remove (get-removable-cells grid 3))
         (num-cells (list-length cells-to-remove)))
    (if (> num-cells 0)
        (progn
          (remove-cells! grid cells-to-remove)
          (+ num-cells (move-all-paper! grid)))
        0)))

(defun solve-p1 (input)
  (let ((grid (get-grid input)))
    (loop for nc in 
                 (loop for coord being the hash-key of grid
                       collect (count-neighbours grid coord))
          count (<= nc 3))))

(defun solve-p2 (input)
  (let ((grid (get-grid input)))
    (move-all-paper! grid)))

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)
