(defun input ()
  (loop for line in (uiop:read-file-lines "day10.input")
        collecting nil
        unless (equal line "noop")
          collecting (parse-integer (subseq line 5))))

(defun simulate (ops)
  (loop with x = 1
        for op in ops
        collect x
        when op do
          (incf x op)))

(defun solution-1 (history)
  (loop for m in '(20 60 100 140 180 220)
        summing (* m (nth (1- m) history))))

(defun solution-2 (history)
  (loop for n from 0
        for i = (mod n 40)
        for x in history
        do (format t "~a" (if (<= (1- x) i (1+ x)) "#" "."))
        if (= i 39) do (format t "~%")))

(let ((history (simulate (input))))
  (format t "~a~%" (solution-1 history))
  (solution-2 history))
;; => 12880
;; => ####..##....##..##..###....##.###..####.
;; => #....#..#....#.#..#.#..#....#.#..#.#....
;; => ###..#.......#.#..#.#..#....#.#..#.###..
;; => #....#.......#.####.###.....#.###..#....
;; => #....#..#.#..#.#..#.#....#..#.#.#..#....
;; => #.....##...##..#..#.#.....##..#..#.####.
