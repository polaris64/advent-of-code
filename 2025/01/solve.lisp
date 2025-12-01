(defpackage :p64/aoc/2025/01
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/01)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (loop :for line := (read-line s nil nil)
          :while line
          :collect line)))

(defun parse-rotation (rot)
  (values
   (let ((direction (nth 0 (coerce rot 'list))))
     (cond ((eq direction #\L) -1)
           ((eq direction #\R) 1)))
   (parse-integer (subseq rot 1 (length rot)))))

(defun rotate (start-pos direction amount)
  (mod (+ start-pos (* direction amount)) 100))

(defun get-times-past-zero (start-pos direction amount)
  (let ((res (abs (floor (/ (+ start-pos (* direction amount)) 100)))))
    (cond ((and (< direction 0) (= start-pos 0)) (1- res))
          ((and (< direction 0) (= (rotate start-pos direction amount) 0)) (1+ res))
          (t res))))

(defun simulate (rotations start-pos)
  (reduce (lambda (a rot)
            (multiple-value-bind (direction amount) (parse-rotation rot)
              (let* ((curr-pos (caddr a))
                     (times-at-zero (car a))
                     (times-passing-zero (cadr a))
                     (new-pos (rotate curr-pos direction amount)))

                (list

                 ;; Number of times the dial stopped on 0
                 (if (eq new-pos 0) (1+ times-at-zero) times-at-zero)

                 ;; The number of times that the dial passes 0 from the current to new position
                 (+ times-passing-zero
                    (get-times-past-zero curr-pos direction amount))

                 ;; The new dial position
                 new-pos))))
          rotations
          :initial-value (list 0 0 start-pos)))

(defun solve-p1 (input)
  (car (simulate input 50)))

(defun solve-p2 (input)
  (cadr (simulate input 50)))

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)
