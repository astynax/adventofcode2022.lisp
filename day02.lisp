(defun abc->rps (x)
  (case x
    (#\A 'r)
    (#\B 'p)
    (#\C 's)))

(defun xyz->rps (x)
  (case x
    (#\X 'r)
    (#\Y 'p)
    (#\Z 's)))

(defun input ()
  (loop for line in (uiop:read-file-lines "day02.input")
        collect (list (abc->rps (aref line 0))
                      (xyz->rps (aref line 2)))))

(defmacro ~ (o m)
  `(and (eq other ,o) (eq me ,m)))

(defun play-round (other me)
  (cond ((eq other me) 'draw)
        ((~ 'r 'p) 'won)
        ((~ 'p 's) 'won)
        ((~ 's 'r) 'won)
        (t 'lose)))

(defun rps->score (r)
  (case r
    (r 1)
    (p 2)
    (s 3)))

(defun round->score (r)
  (case r
    (lose 0)
    (draw 3)
    (won 6)))

(defun score-1 (r)
  (+ (rps->score (second r))
     (round->score (play-round (first r) (second r)))))

(defun rps->round (r)
  (case r
    (r 'lose)
    (p 'draw)
    (s 'won)))

(defun score-2 (r)
  (let* ((o (first r))
         (g (rps->round (second r)))
         (m (loop for x in '(r p s)
                  if (eq g (play-round o x))
                    return x)))
    (+ (rps->score m)
       (round->score g))))

(defun total-score-1 (rounds)
  (loop for r in rounds
        sum (score-1 r)))

(defun total-score-2 (rounds)
  (loop for r in rounds
        sum (score-2 r)))

(let ((rs (input)))
  (print (total-score-1 rs))   ; => 14297
  (print (total-score-2 rs)))  ; => 10498
