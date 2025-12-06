(defpackage :p64/aoc/2025/06
  (:use :cl)
  (:export #:read-input
           #:read-input-p2
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/06)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (loop :for line := (read-line s nil nil)
          :while line
          :collect (remove-if (lambda (v) (= 0 (length v)))
                              (uiop:split-string line)))))

(defun read-input-p2 (filename)
  (with-open-file (s filename
                     :direction :input)
    (loop :for line := (read-line s nil nil)
          :while line
          :collect (coerce line 'list))))

(defun transpose (lists)
  (let ((cols (list-length (car lists))))
    (loop for col from 0 to (1- cols)
          collect (mapcar (lambda (v) (nth col v))
                          lists))))

(defun process-problem (list)
  (let ((numbers (mapcar #'parse-integer (butlast list)))
        (op (car (last list))))
    (cons (cond ((equal op "*") #'*)
                ((equal op "+") #'+))
          numbers)))

(defun process-p2-input (inp)
  (let ((problems (loop with sublist
                        for l in
                              (append 
                               (mapcar (lambda (v)
                                         (if (every (lambda (ch) (eq ch #\Space)) v)
                                             :separator
                                             v))
                                       (transpose inp))
                               (list :separator))
                        if (eq l :separator)
                          collect (nreverse sublist) and do (setf sublist nil)
                        else
                          do (push l sublist))))
    (mapcar (lambda (problem)
              (let ((op (car (last (car problem)))))
                (cons (cond ((eq op #\*) #'*)
                            ((eq op #\+) #'+))
                      (mapcar (lambda (p) (parse-integer (coerce (butlast p) 'string))) problem))))
            problems)))

(defun solve-problem (p)
  (apply (car p) (cdr p)))

(defun solve-p1 (input)
  (apply #'+
         (mapcar #'solve-problem
                 (mapcar #'process-problem
                         (transpose input)))))

(defun solve-p2 (input)
  (apply #'+
         (mapcar #'solve-problem
                 (process-p2-input input))))

(defun run ()
  (format t "The solution to part 1 is: ~a~%" (solve-p1 (read-input "input.txt")))
  (format t "The solution to part 2 is: ~a~%" (solve-p2 (read-input-p2 "input.txt"))))

(run)
