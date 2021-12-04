(defun parse-triangle (line)
  "Convert a LINE of text containing 3 numbers to a list."
  (with-input-from-string (in line)
    (loop for side = (read in nil nil) while side collect side)))

(defun read-all-triangles (stream)
  "Return all triangles by parsing lines from STREAM."
  (loop for line = (read-line stream nil nil) while line collecting (parse-triangle line)))

(defun read-input (filename)
  "Return a list of triangles from FILENAME."
  (with-open-file (in filename)
    (read-all-triangles in)))

(defun valid-triangle-p (triangle)
  "Test if TRIANGLE is valid.

A triangle is valid if the sum of the two shorter sides is larger than
the longest side."
  (let* (
         ;; Enumerate sides (e.g. (1 2 3) => ((0 1) (1 2) (2 3)))
         (enumerated-sides (loop for num in triangle and idx from 0 collecting `(,idx ,num)))

         ;; Find the longest side (idx len)
         (longest-side (reduce #'(lambda (x y) (if (> (second x) (second y)) x y))
                               enumerated-sides
                               :initial-value '(0 0)))

         ;; Remove longest side by index
         (other-sides (remove-if (lambda (x) (= (first longest-side) (first x))) enumerated-sides))

         ;; Calculate the sum of the two shortest sides (second
         ;; element as first is the index)
         (other-sides-len (reduce #'+ other-sides :key #'second)))

    (> other-sides-len (second longest-side))))

(defun solve-p1 ()
  "Solve part 1 of the puzzle and return the solution."
  (let ((triangles (read-input "input.txt")))
    (length (remove-if-not #'valid-triangle-p triangles))))

(defun main ()
  "Solve all parts of the puzzle and print the solutions."
  (format t "The solution to part 1 is: ~a~%" (solve-p1)))
