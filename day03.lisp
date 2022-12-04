(defun input ()
  (loop for line in (uiop:read-file-lines "day03.input")
        collect (concatenate 'list line)))

(defun priority (c)
  ;; (map 'list #'priority "azAZ")
  ;; => (1 26 27 52)
  (let ((cc (char-code c)))
    (if (>= cc 97) (- cc 96) (- cc 38))))

(defun common-item (rucksack)
  (let* ((h (/ (length rucksack) 2))
         (c1 (subseq rucksack 0 h))
         (c2 (subseq rucksack h)))
    (car (intersection c1 c2))))

(defun solution-1 (rucksacks)
  (loop for r in rucksacks
        sum (priority (common-item r))))

(defun triples (a-list)
  (do (acc
       (cursor a-list (subseq cursor 3)))
      ((null (cdddr cursor)) (reverse (cons cursor acc)))
    (push (subseq cursor 0 3) acc)))

(defun intersection-of-all (sets)
  (unless (null sets)
    (let ((i (car sets)))
      (dolist (s (cdr sets) i)
        (setq i (intersection i s))))))

(defun solution-2 (rucksacks)
  (loop for triple in (triples rucksacks)
        sum (priority (car (intersection-of-all triple)))))

(let ((rs (input)))
  (print (solution-1 rs))
  (print (solution-2 rs)))
;; => 8202
;; => 2864
