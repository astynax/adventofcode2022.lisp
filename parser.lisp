(defun skip (sample)
  #'(lambda (source)
      (values 'skip (subseq source (length sample)))))

(defun num (source)
  (multiple-value-bind (res len)
      (parse-integer source :junk-allowed t)
    (values res (subseq source len))))

;; (parse "123,456" #'num (skip ",") #'num)
;; (123 456), ""
(defun parse (source &rest steps)
  (loop with src = source
        with result = nil
        for step in steps
        do (multiple-value-bind (val rest)
               (funcall step src)
             (setf src rest)
             (unless (eql 'skip val)
               (push val result)))
        finally (return (values (reverse result)
                                src))))
