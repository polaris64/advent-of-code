(defpackage :p64/aoc/2025/07
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/07)

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
    (loop :for line := (read-line s nil nil)
          :while line
          :collect (coerce (coerce line 'list) 'vector))))

(defun simulate! (grid)
  (loop for prev-row in grid
        for row in (cdr grid)
        for y from 1
        do (loop for cell across row
                 for x from 0
                 do (cond ((and (not (eq cell #\^))
                                (or (eq (svref prev-row x) #\S)
                                    (eq (svref prev-row x) #\|)))
                           (setf (svref row x) #\|))
                          ((eq cell #\^)
                           (setf (svref row (1- x)) #\|)
                           (setf (svref row (1+ x)) #\|))
                        )))
  grid)

(defun format-grid (grid &optional stream)
  (format stream "~{~a~%~}" (mapcar (lambda (row) (coerce row 'string)) grid)))

(defun count-splits (grid)
  (apply #'+
         (loop for prev-row in grid
               for row in (cdr grid)
               collect (loop for cell across row
                             for x from 0
                             count (and (eq cell #\^)
                                        (eq (svref prev-row x) #\|))))))

(defun simulate-universes (grid)
  (let ((cache (make-hash-table :test #'equal)))

    (labels ((rec (start-x start-y)

               (let* ((cache-key (cons start-x start-y))
                      (cache-value (gethash cache-key cache)))

                 (if cache-value
                     cache-value

                     ;; Find the splitter below this point or the
                     ;; bottom of the grid
                     (let ((new-y (loop for row in (subseq grid (1+ start-y))
                                        for y from start-y
                                        when (or (= y (1- (list-length grid)))
                                                 (eq (svref row start-x) #\^))
                                          return y)))

                       (if new-y

                           ;; Either sum the number of paths from this split
                           (let ((res (+
                                       (rec (1- start-x) new-y)
                                       (rec (1+ start-x) new-y))))
                             (setf (gethash cache-key cache) res)
                             res)

                           ;; Or return as there are no further paths
                           1))))))

      ;; Start the process from the "S" in the first row
      (let ((start-x (loop for cell across (car grid)
                           for x from 0
                           when (eq cell #\S)
                             return x)))
        (rec start-x 1)))))

(defun solve-p1 (input)
  (count-splits (simulate! input)))

(defun solve-p2 (input)
  (simulate-universes input))

(defun run ()
  (format t "The solution to part 1 is: ~a~%" (solve-p1 (read-input "input.txt")))
  (format t "The solution to part 2 is: ~a~%" (solve-p2 (read-input "input.txt"))))

(run)
