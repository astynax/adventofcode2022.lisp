(ql:quickload "alexandria")

(defun input ()
  (concatenate 'list (uiop:read-file-string "day06.input")))

(defun find-mark (size data)
  (loop for i from size
        for l on data
        if (alexandria.2:setp (ignore-errors (subseq l 0 size)))
          do (return i)))

(let ((i (input)))
  (print (find-mark 4 i))
  (print (find-mark 14 i)))
;; => 1848
;; => 2308
