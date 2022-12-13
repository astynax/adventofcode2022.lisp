(load "util.lisp")

(defun decode-line (line)
  (with-input-from-string
      (s (string-replace line '((#\[ . #\() (#\] . #\)) (#\, . #\SPACE))))
    (read s)))

(defun decode (lines)
  (loop until (null lines)
        collect (cons (decode-line (first lines))
                      (decode-line (second lines)))
        do (setf lines (cdddr lines))))

(defun input ()
  (decode (uiop:read-file-lines "day13.input")))

(defun check (l r)
  (cond ((and (listp l) (listp r))
         (cond ((and (null l) (null r)) 'undecidable)
               ((null l) t)
               ((null r) nil)
               (t (destructuring-bind (lh . ls) l
                    (destructuring-bind (rh . rs) r
                      (let ((res (check lh rh)))
                        (if (eql res 'undecidable)
                            (check ls rs)
                            res)))))))
        ((and (numberp l) (numberp r))
         (cond ((= l r) 'undecidable)
               ((< l r) t)
               (t nil)))
        ((numberp l) (check (list l) r))
        ((numberp r) (check l (list r)))
        (t (error "Bad input: ~s VS ~s~%" l r))))

(defun in-orderp (left-packet right-packet)
  (let ((res (check left-packet right-packet)))
    (if (eql res 'undecidable) (error "Undecidable: ~s VS ~s~%"
                                      left-packet right-packet)
        res)))

(defun solution-1 (pairs)
  (loop for i from 1
        for (l . r) in pairs
        if (in-orderp l r)
          sum i))

(defun solution-2 (pairs)
  (let ((xs (sort
             (append '(((2)) ((6)))
                     (loop for (l . r) in pairs
                           collect l
                           collect r))
             #'in-orderp)))
    (loop with p2 = nil
          with p6 = nil
          for i from 1
          for x in xs
          when (equal x '((2)))
            do (setf p2 i)
          when (equal x '((6)))
            do (setf p6 i)
          finally (return (* p2 p6)))))

(solution-1 (input))
;; => 4643
(solution-2 (input))
;; => 21614
