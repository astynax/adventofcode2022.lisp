(defun input ()
  (let ((chunks (list nil)))
    (dolist (line (uiop:read-file-lines "day01.input")
                  (reverse (remove-if #'null chunks)))
      (if (zerop (length line))
          (push nil chunks)
          (push (read-from-string line) (car chunks))))))

(defun sum (xs)
  (loop for v in xs
        sum v))

(defun solution1 (xs)
  (loop for x in xs
        maximize (sum x)))

(defun solution2 (xs)
  (sum
   (subseq
    (sort (loop for x in xs
                collect (sum x))
          #'>)
    0 3)))

(let ((xs (input)))
  (print (solution1 xs))   ; => 70720
  (print (solution2 xs)))  ; => 207148
