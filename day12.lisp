(defclass field ()
  ((width  :type integer :initarg :width)
   (height :type integer :initarg :height)
   (cells  :type array   :initarg :cells)))

(defun decode (lines)
  (loop with width = (length (first lines))
        with height = (length lines)
        with arr = (make-array (list width height))
        with start = nil
        with end = nil
        for y from 0
        for row in lines
        do (loop for x from 0
                 for c across row
                 do (setf (aref arr x y) c)
                 when (eql c #\S) do
                   (setf start (cons x y))
                   (setf (aref arr x y) #\a)
                 when (eql c #\E) do
                   (setf end (cons x y))
                   (setf (aref arr x y) #\z))
        finally (return
                  (list start end
                        (make-instance
                         'field :width width :height height :cells arr)))))

(defun input ()
  (decode (uiop:read-file-lines "day12.input")))

(defun neibs (a-field pos)
  (destructuring-bind (x . y) pos
    (with-slots (width height cells) a-field
      (loop with max-x = (1- width)
            with max-y = (1- height)
            for (dx . dy) in '((-1 . 0) (1 . 0) (0 . -1) (0 . 1))
            for xx = (+ x dx)
            for yy = (+ y dy)
            when (and (<= 0 xx max-x)
                      (<= 0 yy max-y))
              collect (cons (cons xx yy) (aref cells xx yy))))))

(defun levi (pos1 pos2)
  (destructuring-bind (x1 . y1) pos1
    (destructuring-bind (x2 . y2) pos2
      (+ (abs (- x2 x1)) (abs (- y2 y1))))))

(defun dijkstra (fork weight start)
  (loop with distances = (make-hash-table :test #'equal)
        do (setf (gethash start distances) 0)
        with previous = (make-hash-table :test #'equal)
        with queue = (list (cons 0 start))
        until (null queue) do
          (loop with (best . q) = (sort queue #'< :key #'car)
                with (dist . pos-from) = best
                for pos-to in (funcall fork pos-from)
                for w = (funcall weight pos-from pos-to)
                for d = (+ dist w)
                for od = (gethash pos-to distances most-positive-fixnum)
                if (< d od) do
                  (setf q (cons (cons d pos-to)
                                (remove pos-to q
                                        :key #'cdr
                                        :test #'equal)))
                  (setf (gethash pos-to distances) d)
                  (setf (gethash pos-to previous) pos-from)
                finally (setf queue q))
        finally (return previous)))

(defun fork-neibs (a-field pos)
  (loop with arr = (slot-value a-field 'cells)
        with (x . y) = pos
        with v = (1+ (char-code (aref arr x y)))
        for (p . w) in (neibs a-field pos)
        if (<= (char-code w) v)
          collect p))

(defun search-path (a-field start end)
  (let ((m (dijkstra #'(lambda (pos) (fork-neibs a-field pos))
                     #'(lambda (p1 p2) (declare (ignore p1 p2)) 1)
                     start)))
    (loop with pos = end
          with path = nil
          until (null pos) do
            (let ((step (gethash pos m)))
              (setf pos step)
              (push step path))
          finally (return (cdr path)))))

(defvar example '("Sabqponm"
                  "abcryxxl"
                  "accszExk"
                  "acctuvwj"
                  "abdefghi"))

(defun solution-1 (inp)
  (destructuring-bind (start end a-field) inp
    (length (search-path a-field start end))))

(defun field-search (a-field value)
  (with-slots (width height cells) a-field
    (loop for y from 0 below height
          nconcing (loop for x from 0 below width
                         if (eql (aref cells x y) value)
                           collect (cons x y)))))

(defun solution-2 (inp)
  (destructuring-bind (_ end a-field) inp
    (declare (ignore _))
    (loop with starts = (loop for pos in (field-search a-field #\a)
                              for bs = (find #\b (neibs a-field pos)
                                             :key #'cdr)
                              unless (null bs)
                                collect pos)
          for start in starts
          minimizing (length (search-path a-field start end)))))

(solution-1 (input))
;; => 462

(solution-2 (input))
;; => 451
