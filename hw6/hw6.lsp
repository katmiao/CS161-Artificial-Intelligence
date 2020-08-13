; Katherine Miao
; CS161 HW6: Graph coloring to SAT conversion

(defun reload()
  (load "hw6.lsp"))

; ----------------------------------------------------- part 1

; NODE2VAR: returns index of variable that corresponds to 
;   "node n gets color c" (with k possible colors).
;   variable index = (n-1)*k + c
(defun node2var (n c k)
  (+ c (* k (- n 1))))

; ----------------------------------------------------- part 2

; AT-LEAST-ONE-COLOR: returns a clause for constraint:
;   "node n gets at least one color from the set {c,c+1,...,k}"
(defun at-least-one-color (n c k)
  (cond
    ((> c k) NIL)
    (t (cons (node2var n c k) (at-least-one-color n (+ c 1) k)))))
;(print (at-least-one-color 1 1 3))

; ----------------------------------------------------- part 3
; GENERATE-AT-MOST: returns list of clauses such that for all pairs of color
;   and colors between color and k (denoted by c), at most one can be true
(defun generate-at-most (n color c k)
  (cond
    ((> c k) NIL)
    (t (cons (list (- (node2var n color k)) (- (node2var n c k))) (generate-at-most n color (+ c 1) k)))))
;(print (generate-at-most 1 1 2 3))

; AT-MOST-ONE-COLOR: returns list of clauses for constraint:
;   "node n gets at most one color from the set {c,c+1,...,k}."
(defun at-most-one-color (n c k)
  (cond
    ((> c k) NIL)
    ((< k 1) NIL)
    (t (append (generate-at-most n c (+ c 1) k) (at-most-one-color n (+ c 1) k)))))
;(print (at-most-one-color 1 1 3))

; ----------------------------------------------------- part 4

; GENERATE-NODE-CLAUSES: returns list of clauses for constraint:
;   "node n gets exactly one color from the set {1,2,...,k}."
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k)))
;(print (generate-node-clauses 1 5))

; ----------------------------------------------------- part 5

; EDGES-DIFF-COLOR: returns constraint such that
;   node x and node y do not have the same color.
;   This is NOT( (x has color) AND (y  has color)),
;   i.e. (x does not have color) OR (y does not have color)
(defun edges-diff-color (x y k color)
  (list (- (node2var x color k)) (- (node2var y color k))))

; EDGES-DIFF-ALL-COLORS: returns list of contraints such that 
;   edges x and y have different colors for all k colors.
(defun edges-diff-all-colors (x y k c)
  (cond 
    ((> c k) NIL)
    (t (cons (edges-diff-color x y k c) 
             (edges-diff-all-colors x y k (+ c 1))))))

; GENERATE-EDGE-CLAUSES: returns list of clauses for constraint:
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
(defun generate-edge-clauses (e k)
  (edges-diff-all-colors (first e) (second e) k 1))
;(print (generate-edge-clauses '(1 2) 5))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	       (+ node 1)
	       ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	       (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			 )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	     (format out "~A " (car clause))
	     (write-clause-to-file out (cdr clause))
	     );end progn
	   );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun


;(graph-coloring-to-sat "graph2.txt" "result8.txt" 8)