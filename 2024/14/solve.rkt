#lang racket

(require pict)

(define test-input "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
")

(define (read-input)
  (call-with-input-file "input.txt"
    (lambda (port)
      (port->string port))))

(define (make-vec x y) (cons x y))

(define (vec-x vec) (car vec))

(define (vec-y vec) (cdr vec))

(define (make-robot start-pos velocity) (cons start-pos velocity))

(define (robot-start-pos robot) (car robot))

(define (robot-velocity robot) (cdr robot))

(define (parse-input input-str)
  (map (λ (line)
         (let ((res (map string->number
                         (cdr 
                          (regexp-match #rx"^p=(-?[0-9]+),(-?[0-9]+) v=(-?[0-9]+),(-?[0-9]+)" line)))))
           (make-robot (make-vec (list-ref res 0)
                                 (list-ref res 1))
                       (make-vec (list-ref res 2)
                                 (list-ref res 3)))))
       (string-split input-str "\n")))

(define (get-final-position robot steps w h)
  (make-vec
   (modulo (+ (* (vec-x (robot-velocity robot))
                 steps)
              (vec-x (robot-start-pos robot)))
           w)
   (modulo (+ (* (vec-y (robot-velocity robot))
                 steps)
              (vec-y (robot-start-pos robot)))
           h)))

(define (get-final-positions robots steps w h)
  (map (λ (robot) (get-final-position robot steps w h)) robots))

(define (group-positions positions)
  (map (λ (group)
         (cons (car group) (length group)))
       (group-by identity positions)))

(define (split-positions positions w h)
  (let ((split-x (floor (/ w 2)))
        (split-y (floor (/ h 2))))
    (let ((q1 (filter (λ (v)
                        (and (>= (vec-x (car v)) 0)
                             (< (vec-x (car v)) split-x)
                             (>= (vec-y (car v)) 0)
                             (< (vec-y (car v)) split-y)))
                      positions))
          (q2 (filter (λ (v)
                        (and (> (vec-x (car v)) split-x)
                             (< (vec-x (car v)) w)
                             (>= (vec-y (car v)) 0)
                             (< (vec-y (car v)) split-y)))
                      positions))
          (q3 (filter (λ (v)
                        (and (>= (vec-x (car v)) 0)
                             (< (vec-x (car v)) split-x)
                             (> (vec-y (car v)) split-y)
                             (< (vec-y (car v)) h)))
                      positions))
          (q4 (filter (λ (v)
                        (and (> (vec-x (car v)) split-x)
                             (< (vec-x (car v)) w)
                             (> (vec-y (car v)) split-y)
                             (< (vec-y (car v)) h)))
                      positions))
          )
      (list q1 q2 q3 q4))))

(define (get-quadrant-sum quadrant)
  (apply + (map (λ (robot) (cdr robot)) quadrant)))

(define (get-safety-factor quadrants)
  (apply * (map get-quadrant-sum quadrants)))

(define (visualise-grid robots w h)
  (apply vl-append
         (map (λ (y)
                (apply hc-append
                       (map (λ (x)
                              (colorize
                               (filled-rectangle 1 1)
                               (if (member (make-vec x y) robots) "blue" "white")))
                            (range w))))
              (range h))))

;; Find the number of steps needed for the robots to arrange
;; themselves into a Christmas tree pattern.
;; This assumes that this pattern was the starting point for generating
;; the challenge and therefore all robots were in unique positions. The
;; result is the number of steps in order for all robots to be in unique
;; positions.
(define (find-christmas-tree inp start-steps w h)
  (let ((l (length inp)))
    (define (iter steps)

      ;; LCM (w, h) is the upper bound before robots will wrap back to
      ;; the starting positions. As all grids in this challenge have
      ;; dimensions which are primes, LCM is simply the product
      (cond ((> steps (* w h)) #f)
            ((= l (count (λ (v)
                           (= 1 (cdr v)))
                         (group-positions (get-final-positions inp
                                                               steps
                                                               w
                                                               h))))
             steps)
            (else (iter (+ 1 steps)))))
    (iter start-steps)))

(define (visualise-christmas-tree inp w h)
  (let* ((inp (parse-input inp))
         (steps (find-christmas-tree inp 0 w h)))
    (when steps
      (visualise-grid (get-final-positions inp steps w h) w h))))

;; (visualise-christmas-tree (read-input) 101 103)

(define (solve-p1 inp steps w h)
  (get-safety-factor
   (split-positions 
    (group-positions
     (get-final-positions (parse-input inp) steps w h))
    w h)))

(define (solve-p2 inp)
  (find-christmas-tree (parse-input inp) 0 101 103))

(define (compare-results part actual expected)
  (when (not (equal? actual expected))
    (error (format #f "Part ~a: solution is not correct: ~a != ~a" part actual expected)))
  actual)

(define (report-result part result)
  (display
   (format "The solution to part ~a is: ~a~%" part result)))

(define (report-and-compare-results part actual expected)
  (report-result part actual)
  (compare-results part actual expected))

(define (run-tests)
  (report-and-compare-results 1 (solve-p1 test-input 100 11 7) 12))

(define (run)
  (let ((inp (read-input)))
    (report-and-compare-results 1 (solve-p1 inp 100 101 103) 218619120)
    (report-and-compare-results 2 (solve-p2 inp) 7055)))
