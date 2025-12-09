(ql:quickload "alexandria")

(defpackage :p64/aoc/2025/08
  (:use :cl)
  (:export #:read-input
           #:solve-p1
           #:solve-p2))

(in-package :p64/aoc/2025/08)

(defstruct box id x y z)

(defun list-to-box (id l)
  (make-box :id id
            :x (nth 0 l)
            :y (nth 1 l)
            :z (nth 2 l)))

(defun read-input (filename)
  (with-open-file (s filename
                     :direction :input)
                  (loop :for line := (read-line s nil nil)
                        :for i from 0
                        :while line
                        :collect (list-to-box i (mapcar #'parse-integer
                                                        (uiop:split-string line :separator '(#\,)))))))

(defun calc-distance (p1 p2)
  (sqrt (+ (expt (- (box-x p1) (box-x p2)) 2)
           (expt (- (box-y p1) (box-y p2)) 2)
           (expt (- (box-z p1) (box-z p2)) 2))))

(defun get-distances (boxes)
  (loop for c1 in (subseq boxes 0 (- (list-length boxes) 2))
        for idx1 from 0
        append (loop for c2 in (subseq boxes (1+ idx1))
                      collect (list c1 c2 (calc-distance c1 c2)))))

(defun get-closest-boxes (boxes &optional n)
  (let ((distances (subseq (sort (get-distances boxes) #'< :key (lambda (v) (nth 2 v))) 0 n)))
    (if n
        (subseq distances 0 n)
        distances)))

(defun get-box-circuit (box circuits)
  (loop for cid being the hash-key of circuits
        for l being the hash-value of circuits
        when (some (lambda (b) (= b (box-id box))) l)
          return cid))

(defun build-circuits (boxes shortest-distances &optional check-last-fn)
  (let ((circuits (make-hash-table)))

    ;; Create all circuits for each junction box
    (loop for v in boxes
          for i from 0
          do (setf (gethash i circuits) (list (box-id v))))

    ;; Connect all junction boxes in `shortest-distances'
    (let ((res
            (loop for v in shortest-distances
                  do (let* ((b1 (car v))
                            (b1-circuit (get-box-circuit b1 circuits))
                            (b2 (cadr v))
                            (b2-circuit (get-box-circuit b2 circuits)))

                       ;; Check if b1 and b2 are already in the same circuit.
                       ;; If so, do nothing.
                       (when (not (= b1-circuit b2-circuit))

                         ;; Move all boxes from b2-circuit to b1-circuit
                         (setf (gethash b1-circuit circuits)
                               (append (gethash b1-circuit circuits) (gethash b2-circuit circuits)))
                         (setf (gethash b2-circuit circuits) nil))

                       ;; If there's a check-last-fn, call it with the current
                       ;; circuits and return from the loop if it returns t.
                       (when (and check-last-fn (funcall check-last-fn circuits))
                         (return (list circuits b1 b2)))))))

      (if res (apply #'values res)
          (values circuits nil nil)))))

(defun get-n-biggest-circuits (n circuits)
  (subseq 
   (sort (loop for v being the hash-value of circuits collect (list-length v)) #'>)
   0
   n))

(defun solve-p1 (input n-biggest n-closest)
  (apply
   #'*
   (get-n-biggest-circuits
    n-biggest
    (build-circuits
     input
     (get-n-closest-boxes n-closest input)))))

(defun solve-p2 (input)
  (multiple-value-bind
        (circuits last-b-1 last-b-2)
      (build-circuits input
                      (get-closest-boxes input)
                      (lambda (circuits)
                        (when (= (1- (list-length input))
                               (loop for v being the hash-value of circuits
                                     when (null v)
                                       count 1))
                            t)))
    (declare (ignore circuits))
    (* (box-x last-b-1) (box-x last-b-2))))

(defun run ()
  (let ((inp (read-input "input.txt")))
    (format t "The solution to part 1 is: ~a~%" (solve-p1 inp 3 1000))
    (format t "The solution to part 2 is: ~a~%" (solve-p2 inp))))

(run)
