(load "util.lisp")
(load "parser.lisp")

;; (decode-line "Sensor at x=2, y=18: closest beacon is at x=-2, y=15")
;; => ((2 . 18) -2 . 15)
(defun decode-line (line)
  (destructuring-bind (sx sy bx by)
      (parse line
             (p/skip (p/string "Sensor at x=")) p/num
             (p/skip (p/string ", y=")) p/num
             (p/skip (p/string ": closest beacon is at x=")) p/num
             (p/skip (p/string ", y=")) p/num)
    (cons (cons sx sy)
          (cons bx by))))

(defun input (&optional (ext ".input"))
  (loop for line in (uiop:read-file-lines
                     (concatenate 'string "day15" ext))
        collect (decode-line line)))

;; (interval-at 5 '((3 . 0) . (3 . 10)))
;; => (-2 . 8)
(defun interval-at (y sensor)
  (destructuring-bind (ps . pb) sensor
    (destructuring-bind (sx . sy) ps
      (let ((dist (manhattan ps pb))
            (dy (abs (- sy y))))
        (unless (> dy dist)
          (cons (+ (- sx dist) dy)
                (- (+ sx dist) dy)))))))

;; (merge-intervals '((1 . 5) (7 . 10) (3 . 8) (9 . 9) (12 . 20)))
;; => ((1 . 10) (12 . 20))
(defun merge-intervals (intervals)
  (loop with ints = (sort intervals #'< :key #'car)
        with result = nil
        until (null ints)
        for (f0 . t0) = (first ints)
        for nt = (loop for (f1 . t1) in ints
                       when (<= f1 t0)
                         maximizing t1)
        for rest = (loop for i in ints
                         when (> (car i) t0)
                           collect i)
        do (cond ((equal t0 nt)
                  (push (cons f0 t0) result)
                  (setf ints rest))
                 (t (setf ints (cons (cons f0 nt) rest))))
        finally (return (reverse result))))

(defun coverage-at (y sensors)
  (let ((intervals (loop for s in sensors
                         for i = (interval-at y s)
                         unless (null i)
                           collect i)))
    (merge-intervals intervals)))

;; (coverage-at 10 (input ".example"))
;; => ((-2 . 24))

;; (solution-1 (input))
;; => 5166077
(defun solution-1 (sensors)
  (loop for (from . to) in (coverage-at 2000000 sensors)
        summing (- to from)))

;; (solution-2 (input ".example"))
;; => 56000011
;; (solution-2 (input ".example"))
;; => 13071206703981
(defun solution-2 (sensors)
  (loop for y from 0 to 4000000
        for ints = (coverage-at y sensors)
        when (> (length ints) 1) do
          (return
            (let ((i (cdar ints)))
              (+ (* 4000000 (1+ i))
                 y)))))
