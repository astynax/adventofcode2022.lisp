(load "util.lisp")

(defun decode-stacks (a-list)
  (let* ((ls (cdr (reverse a-list)))
         (w (floor (/ (1+ (length (car ls))) 4)))
         (a (make-array (list w) :initial-element (list))))
    (dolist (l ls a)
      (loop for i from 0 below w do
        (let ((c (aref l (1+ (* i 4)))))
          (unless (equal c #\SPACE)
            (setf (aref a i) (cons c (aref a i)))))))))

(defun decode-move (s)
  (destructuring-bind (_move n _from from _to to)
      (uiop:split-string s :separator '(#\SPACE))
    (declare (ignore _move _from _to))
    (list (parse-integer n)
          (1- (parse-integer from))
          (1- (parse-integer to)))))

(defun input ()
  (destructuring-bind (stacks moves)
      (split-on #'(lambda (x) (equal x ""))
                (uiop:read-file-lines "day05.input"))
    (list (decode-stacks stacks)
          (mapcar #'decode-move (cdr moves)))))

(defun stack-tops (stacks)
  (map 'string #'car stacks))

(defun move (stacks a-move &key (one-by-one nil))
  (destructuring-bind (n from to) a-move
    (let ((chunk (subseq (aref stacks from) 0 n)))
      (setf (aref stacks from) (subseq (aref stacks from) n))
      (setf (aref stacks to)
            (append (if one-by-one (reverse chunk) chunk)
                    (aref stacks to)))))
  stacks)

(defun solution (inp &key (one-by-one nil))
  (destructuring-bind (stacks moves) inp
    (let ((s (copy-seq stacks)))
      (dolist (m moves (stack-tops s))
        (move s m :one-by-one one-by-one)))))

(let ((inp (input)))
  (print (solution inp :one-by-one t))
  (print (solution inp)))
;; => "VJSFHWGFT"
;; => "LCTQFBVZV"
