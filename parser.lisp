(defun p/string (sample)
  #'(lambda (source)
      (let* ((l (length sample))
             (prefix (ignore-errors (subseq source 0 l))))
        (cond ((equal prefix sample)
               (values prefix (subseq source l)))
              (t (error "Expected:~% ~s~%Actual:~% ~s" sample source))))))

(defun p/string-of (pred)
  #'(lambda (source)
      (loop for i from 0 below (length source)
            for c = (aref source i)
            unless (funcall pred c)
              do (return (values (subseq source 0 i)
                                 (subseq source i)))
            finally
               (return (values source "")))))

(defvar p/num
  (lambda (source)
    (multiple-value-bind (res len)
        (parse-integer source :junk-allowed t)
      (cond ((null res)
             (error "Expected:~% NUMBER~%Actual:~% ~s~%" source))
            (t (values res (subseq source len)))))))

(defun p/skip (parser)
  #'(lambda (source)
      (multiple-value-bind (_ rest) (funcall parser source)
        (declare (ignore _))
        (values 'skip rest))))

(defun p/progn (parser &rest parsers)
  #'(lambda (source)
      (loop with s = source
            with last-res = nil
            for p in (cons parser parsers)
            do (multiple-value-bind (res rest)
                   (funcall p s)
                 (setf last-res res)
                 (setf s rest))
            finally (return (values last-res s)))))

(defun p/or (p1 p2)
  #'(lambda (source)
      (handler-case (funcall p1 source)
        (t () (funcall p2 source)))))

(defun p/pure (val)
  #'(lambda (source)
      (values val source)))

(defun p/map (f parser)
  #'(lambda (source)
      (multiple-value-bind (res rest) (funcall parser source)
        (values (funcall f res) rest))))

(defun p/map2 (f p1 p2)
  #'(lambda (source)
      (multiple-value-bind (r1 rest1) (funcall p1 source)
        (multiple-value-bind (r2 rest2) (funcall p2 rest1)
          (values (funcall f r1 r2) rest2)))))

(defun p/many (parser)
  #'(lambda (source)
      (funcall (p/or (p/map2 #'cons parser (p/many parser))
                     (p/pure nil))
               source)))

(defun p/sep-by-1 (separator parser)
  (p/map2 #'cons parser (p/many (p/progn separator parser))))

(defun p/sep-by (separator parser)
  (p/or (p/sep-by-1 separator parser)
        (p/pure nil)))

#+f
(parse "123,456,789foo"
       (p/sep-by (p/string ",") p/num)
       (p/skip (p/string-of #'alpha-char-p)))
;; => ((123 456 789))
#+f
(parse "123,456" p/num (p/skip (p/string ",")) p/num)
;; => (123 456)
(defun parse (source &rest steps)
  (loop with src = source
        with result = nil
        for step in steps
        do (multiple-value-bind (val rest)
               (funcall step src)
             (setf src rest)
             (unless (eql 'skip val)
               (push val result)))
        finally
           (return (if (zerop (length src))
                       (reverse result)
                       (error "Unhandled input:~% ~s~%" src)))))
