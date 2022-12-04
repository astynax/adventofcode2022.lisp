(defun decode-range (s)
  (let* ((pair (uiop:split-string s :max 2 :separator '(#\-)))
         (n1 (parse-integer (first pair)))
         (n2 (parse-integer (second pair))))
    (list n1 n2)))

(defun range->set (r)
  (loop for x from (first r) to (second r)
        collect x))

(defun decode (line)
  (let* ((pair (uiop:split-string line :max 2 :separator '(#\,)))
         (r1 (decode-range (first pair)))
         (r2 (decode-range (second pair))))
    (list (range->set r1)
          (range->set r2))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day04.input")
        collect (decode line)))

(defun solution-1 (pairs)
  (loop for (s1 s2) in pairs
        count (or (subsetp s1 s2) (subsetp s2 s1))))

(defun solution-2 (pairs)
  (loop for (s1 s2) in pairs
        count (not (null (intersection s1 s2)))))

(let ((rs (input)))
  (print (solution-1 rs))
  (print (solution-2 rs)))
;; => 599
;; => 928
