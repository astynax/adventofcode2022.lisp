(defun split-on (test a-list)
  (do ((chunk nil (cons (car cursor) chunk))
       (cursor a-list (cdr cursor)))
      ((or (null cursor)
           (funcall test (car cursor)))
       (list (reverse chunk)
             cursor))))

;; (string-replace "abc." '((#\b . #\.) (#\. . #\b)))
;; => "a.cb"
(defun string-replace (source pairs)
  (loop with result = (concatenate 'string source)
        for i from 0
        for c across source
        do (loop for (from . to) in pairs
                 when (eql from c) do
                   (setf (aref result i) to)
                   (return))
        finally (return result)))
