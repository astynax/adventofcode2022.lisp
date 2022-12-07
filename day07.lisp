(defun decode (line)
  (cond
    ((uiop:string-prefix-p "$ cd " line)
     (list 'cd (let ((d (subseq line 5)))
                 (cond ((equal d "/") 'root)
                       ((equal d "..") 'up)
                       (t d)))))
    ((equal "$ ls" line) '(ls))
    ((uiop:string-prefix-p "dir " line)
     (list 'dir (subseq line 4)))
    (t (destructuring-bind (s n) (uiop:split-string line :max 2)
         (list 'file (parse-integer s) n)))))

(defun input ()
  (loop for line in (uiop:read-file-lines "day07.input")
        collect (decode line)))

(defun make-node ()
  (list (make-hash-table) (list)))

(defun add-file (tree file)
  (destructuring-bind (s n) (cdr file)
    (push (list s n) (second tree))))

(defun make-dir (tree name)
  (let ((dir (make-node)))
    (setf (gethash name (first tree)) dir)
    dir))

(defun build-tree (lines)
  (loop
    with tree = (make-node)
    with path = (list tree)
    finally (return tree)
    for line in lines do
      (case (car line)
        (cd (let ((d (second line)))
              (case d
                (root (setq path (list (car (reverse path)))))
                (up (setq path (cdr path)))
                (t (push (make-dir (car path) d) path)))))
        (ls nil)
        (dir nil)
        (file (add-file (car path) line)))))

(defun print-tree (tree &optional (offset ""))
  (destructuring-bind (ds fs) tree
    (loop for d being each hash-key of ds do
      (format t "~a~a/~%" offset d)
      (print-tree (gethash d ds) (concatenate 'string " " offset)))
    (loop for (s n) in fs do
      (format t "~a~a ~a~%" offset n s))))

(defun sizes (tree)
  (destructuring-bind (ds fs) tree
    (let* ((tf (loop for file in fs
                     sum (first file)))
           (dss (loop for d being each hash-key of ds
                      for ss = (sizes (gethash d ds))
                      nconcing ss into subsizes
                      summing (car ss) into total
                      finally (return (list total subsizes))))
           (td (first dss))
           (subsizes (second dss))
           (total (+ tf td)))
      (cons total subsizes))))

(defun solution-1 (ss)
  (loop for s in ss
        if (<= s 100000)
          sum s))

(defun solution-2 (ss)
  (loop with total = (first ss)
        with delta = (- total 40000000)
        for s in (sort ss #'<)
        if (>= s delta)
          do (return s)))

(let ((ss (sizes (build-tree (input)))))
  (print (solution-1 ss))
  (print (solution-2 ss)))
;; => 1086293
;; => 366028
