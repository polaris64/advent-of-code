(defpackage :p64/aoc/2025/09
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/09)

(defun list-to-point (l)
  (cons (car l) (cadr l)))

(defun read-input (filename)
  (with-open-file (s filename :direction :input)
    (loop for line = (read-line s nil nil)
          while line
          collect (list-to-point (mapcar #'parse-integer (uiop:split-string line :separator '(#\,)))))))

(defun calc-dist (p1 p2)
  (sqrt (+ (expt (- (car p1) (car p2)) 2)
           (expt (- (cdr p1) (cdr p2)) 2))))

(defun calc-area (p1 p2)
  (* (abs (- (car p1) (car p2) (- 1)))
     (abs (- (cdr p1) (cdr p2) (- 1)))))

(defun find-largest-area (points)
  (sort 
   (loop for p1 in points
         for i1 from 0
         append (loop for p2 in (subseq points (1+ i1))
                      collect (list p1 p2 (calc-area p1 p2))))
   #'>
   :key #'caddr))

(defun solve-p1 (input)
  (caddr
   (car (find-largest-area input))))

(defun solve-p2 (input) 0)

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)

#+nil
(progn
  (read-input "input_ex.txt")
  (sort (read-input "input_ex.txt") #'> :key #'car)
   ; => ((11 . 1) (11 . 7) (9 . 7) (9 . 5) (7 . 1) (7 . 3) (2 . 5) (2 . 3))
  (sort (read-input "input_ex.txt") #'< :key #'car)
   ; => ((2 . 5) (2 . 3) (7 . 1) (7 . 3) (9 . 7) (9 . 5) (11 . 1) (11 . 7))
  (sort (read-input "input_ex.txt") #'> :key #'cdr)
   ; => ((11 . 7) (9 . 7) (9 . 5) (2 . 5) (2 . 3) (7 . 3) (7 . 1) (11 . 1))
  (sort (read-input "input_ex.txt") #'< :key #'cdr)
   ; => ((7 . 1) (11 . 1) (2 . 3) (7 . 3) (9 . 5) (2 . 5) (11 . 7) (9 . 7))
  (loop for pt in (read-input "input_ex.txt")
        maximizing (car pt)
        collect pt)
  (let ((points (read-input "input_ex.txt")))
    (reduce (lambda (a v) (if (and (not (null a)) (>= (car v) (apply #'max (mapcar (lambda (p) (car p)) a))))
                              (cons v a)
                              a))
            (cdr points)
            :initial-value (list (car points))))
  (find-largest-area (read-input "input.txt"))
  )
