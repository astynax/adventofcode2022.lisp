(defclass state ()
  ((size :type integer :initarg size)
   (head :type cons :initform (cons 0 0))
   (tail :type vector)
   (trail :type hash-table :initform (make-hash-table :test #'equal))))

(defun make-state (size)
  (make-instance 'state 'size size))

(defmethod initialize-instance :after ((instance state) &key)
  (with-slots (size) instance
    (setf (slot-value instance 'tail)
          (make-array (list size) :initial-element (cons 0 0)))))

;; (follow (cons 4 2) (cons 3 4))
;; => (4 . 3)
(defun follow (head tail)
  (destructuring-bind (hx . hy) head
    (destructuring-bind (tx . ty) tail
      (let ((dx (- hx tx))
            (dy (- hy ty)))
        (if (or (< 1 (abs dx))
                (< 1 (abs dy)))
            (cons (+ tx (signum dx))
                  (+ ty (signum dy)))
            tail)))))

;; (pull-the-tail (cons 4 2) #((3 . 0) (2 . 0) (1 . 0) (0 . 0)))
;; => (1 . 1)
(defun pull-the-tail (head tail)
  (loop with prev = head
        for i from 0
        for current across tail
        for new = (follow prev current)
        do (setf prev new)
           (setf (aref tail i) new)
        finally (return new)))

;; (move (cons 10 10) 'd)
;; => (10 . 9)
(defun move (pos dir)
  (destructuring-bind (x . y) pos
    (ecase dir
      ((u) (cons x (1+ y)))
      ((d) (cons x (1- y)))
      ((r) (cons (1+ x) y))
      ((l) (cons (1- x) y)))))

(defun crawl (a-state dir)
  (with-slots (head tail trail) a-state
    (let* ((new-head (move head dir))
           (old-tail (pull-the-tail new-head tail)))
      (setf (slot-value a-state 'head) new-head)
      (setf (gethash old-tail trail) t))
    a-state))

(defun run (a-state moves)
  (loop for (d . n) in moves
        do (loop repeat n
                 do (crawl a-state d))
        finally (return a-state)))

(defvar example
  '((R . 4)
    (U . 4)
    (L . 3)
    (D . 1)
    (R . 4)
    (D . 1)
    (L . 5)
    (R . 2)))

(defun draw-trail (a-state)
  (with-slots (trail) a-state
    (loop for (x . y) being the hash-keys in trail
          minimizing x into min-x
          maximizing x into max-x
          minimizing y into min-y
          maximizing y into max-y
          finally
             (loop for y from min-y to max-y do
               (loop for x from min-x to max-x do
                 (format t "~a" (if (gethash (cons x y) trail)
                                    #\# #\.)))
               (format t "~%")))))

(defun solve (size moves)
  (hash-table-count
   (slot-value (run (make-state size) moves) 'trail)))

(defun decode (line)
  (cons (ecase (aref line 0)
          ((#\U) 'u) ((#\D) 'd) ((#\L) 'l) ((#\R) 'r))
        (parse-integer (subseq line 2))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day09.input")
        collect (decode line)))

(format t "Example: ~a~%" (solve 1 example))
;; => Example: 13

(let ((moves (input)))
  (print (solve 1 moves))
  (print (solve 9 moves)))
;; => 5902
;; => 2445
