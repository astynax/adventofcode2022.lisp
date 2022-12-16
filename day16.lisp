(load "parser.lisp")

(defvar p/name (p/string-of #'alpha-char-p))

;; (decode-line "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB")
;; => ("AA" 0 ("DD" "II" "BB"))
(defun decode-line (line)
  (parse line
         (p/skip (p/string "Valve "))
         p/name
         (p/skip (p/string " has flow rate="))
         p/num
         (p/skip (p/or (p/string "; tunnels lead to valves ")
                       (p/string "; tunnel leads to valve ")))
         (p/sep-by-1 (p/string ", ") p/name)))

(defun input ()
  (loop for line in (uiop:read-file-lines "day16.input")
        collect (decode-line line)))
