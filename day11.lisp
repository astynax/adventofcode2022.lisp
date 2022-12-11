(defclass monkey ()
  ((items     :type list    :initarg :items)
   (operation :type cons    :initarg :operation)
   (divider   :type integer :initarg :divider)
   (if-true   :type integer :initarg :if-true)
   (if-false  :type integer :initarg :if-false)))

(defmacro chop (prefix var)
  `(let ((h (pop ,var))
         (l (length ,prefix)))
     (unless (equal ,prefix (subseq h 0 l))
       (error "Bad line: ~s" l))
     (subseq h l)))

(defun decode-monkey (lines)
  ;; Monkey 0:
  ;;   Starting items: 74, 73, 57, 77, 74
  ;;   Operation: new = old * 11
  ;;   Test: divisible by 19
  ;;     If true: throw to monkey 6
  ;;     If false: throw to monkey 7
  (list (parse-integer (chop "Monkey " lines) :junk-allowed t)
        (let ((is (chop "  Starting items: " lines))
              (op (chop "  Operation: new = old " lines))
              (di (chop "  Test: divisible by " lines))
              (tr (chop "    If true: throw to monkey " lines))
              (fa (chop "    If false: throw to monkey " lines)))
          (make-instance
           'monkey
           :items (loop for part in (uiop:split-string is :separator '(#\SPACE))
                        collect (parse-integer part :junk-allowed t))
           :operation (cons (if (equal "+" (subseq op 0 1)) #'+ #'*)
                            (let ((arg (subseq op 2)))
                              (if (equal arg "old")
                                  'old
                                  (parse-integer (subseq op 2)))))
           :divider (parse-integer di)
           :if-true (parse-integer tr)
           :if-false (parse-integer fa)))
        lines))

(defun input ()
  (loop with lines = (uiop:read-file-lines "day11.input")
        until (null lines)
        for (i m ls) = (decode-monkey lines)
        collect (cons i m)
        do (setf lines (cdr ls))))

;; (apply-op (cons #'+ 'old) 42)
;; => 84
;; (apply-op (cons #'* 10) 42)
;; => 420
(defun apply-op (operation arg)
  (destructuring-bind (op . arg2) operation
    (funcall op arg (if (eql 'old arg2) arg arg2))))

(defun throw-items (limit a-monkey)
  (with-slots (items operation divider if-true if-false) a-monkey
    (loop for item in items
          for new = (funcall limit (apply-op operation item))
          collecting (cons (if (zerop (mod new divider)) if-true if-false)
                           new) into throws
          finally
             (setf (slot-value a-monkey 'items) nil)
             (return throws))))

(defun catch-intems (monkeys items)
  (loop for (i . item) in items
        for monkey = (cdr (assoc i monkeys))
        do (with-slots (items) monkey
             (setf (slot-value monkey 'items) (append items (list item))))))

(defun run (times limit monkeys)
  (loop with counts = (make-hash-table)
        repeat times
        do (loop for (i . monkey) in monkeys
                 for throws = (throw-items limit monkey)
                 do (incf (gethash i counts 0) (length throws))
                    (catch-intems monkeys throws))
        finally (return counts)))

(defun hash-table-values (table)
  (loop for v being each hash-value in table
        collect v))

(defun div-by-3 (x)
  (floor (/ x 3)))

(defun calculate (counts)
  (apply #'* (subseq (sort (hash-table-values counts) #'>) 0 2)))

(defun solution-1 ()
  (calculate (run 20 #'div-by-3 (input))))

(defun solution-2 ()
  (let* ((monkeys (input))
         (m (apply #'lcm (loop for pair in monkeys
                               collect (slot-value (cdr pair) 'divider))))
         (counts (run 10000 #'(lambda (x) (mod x m)) monkeys)))
    (calculate counts)))

(solution-1)
;; => 69918
(solution-2)
;; => 19573408701
