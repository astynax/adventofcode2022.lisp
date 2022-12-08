(defclass field ()
  ((width :type integer
          :initarg width
          :reader field-width)
   (height :type integer
           :initarg height
           :reader field-height)
   (trees :type array
          :initarg trees
          :reader trees)))

(defmacro field/defun (name args &body body)
  `(defun ,name (a-field ,@args)
     (with-slots (width height trees) a-field
       ,@body)))

(defun input ()
  (loop with lines = (uiop:read-file-lines "day08.input")
        with height = (length lines)
        with width = (length (first lines))
        with arr = (make-array (list width height))
        for y from 0
        for line in lines do
          (loop for x from 0
                for c across line do
                  (setf (aref arr x y) (- (char-code c) 48)))
        finally
           (return (make-instance 'field
                                  'width width
                                  'height height
                                  'trees arr))))

(field/defun look-around (x y)
  (list (aref trees x y)
        (loop for i from (1- x) downto 0     collect (aref trees i y))
        (loop for i from (1+ x) below width  collect (aref trees i y))
        (loop for j from (1- y) downto 0     collect (aref trees x j))
        (loop for j from (1+ y) below height collect (aref trees x j))))

(field/defun perimeter ()
  (- (* 2 (+ width height)) 4))

(defun visible-behindp (h trees)
  (not (loop for tree in trees
             if (>= tree h)
               do (return t))))

(defun visiblep (look)
  (destructuring-bind (h &rest (l r u d)) look
    (or (visible-behindp h l)
        (visible-behindp h r)
        (visible-behindp h u)
        (visible-behindp h d))))

(field/defun visible-interior ()
  (loop for y from 1 below (1- height)
        sum (loop for x from 1 below (1- width)
                  if (visiblep (look-around a-field x y))
                  sum 1)))

(defun visible-distance (h trees)
  (loop with dist = 0
        finally (return dist)
        for tree in trees
        do (if (>= tree h)
               (return (1+ dist))
               (incf dist))))

(defun score (look)
  (destructuring-bind (h &rest (l r u d)) look
    (* (visible-distance h l)
       (visible-distance h r)
       (visible-distance h u)
       (visible-distance h d))))

(defun solution-1 (a-field)
  (+ (perimeter a-field)
     (visible-interior a-field)))

(field/defun solution-2 ()
  (loop for y from 0 below height
        maximize (loop for x from 0 below width
                       maximize (score (look-around a-field x y)))))

(let ((f (input)))
  (print (solution-1 f))
  (print (solution-2 f)))
;; => 1823
;; => 211680
