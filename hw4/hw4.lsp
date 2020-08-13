; Katherine Miao
; CS161 Homework 4

; ISMEMBER -- if item is in list, return true. Otherwise return NIL.
(defun isMember (item L)
  (cond 
    ((not (member item L)) NIL)
    (t t)))

; ISVALIDCLAUSE -- checks if clause is, or could be, valid under this assignment.
;   Validity is met if an element is true or unassigned.
(defun isValidClause (assignment clause)
  (cond 
    ((not clause) NIL)
    (t (let* ((var (first clause)) (absvar (abs var)))
      (cond 
        ; if element is in assignment OR has not been assigned, clause is valid
        ((OR (AND (not (isMember var assignment)) (not (isMember (- var) assignment)))
             (isMember var assignment))
          t)
        (t (isValidClause assignment (rest clause))))))))

; ISVALIDMODEL -- uses isValidClause to check if all clauses in model are valid. 
(defun isValidModel (assignment delta)
  (cond
    ((not delta) t)
    ((= (length delta) 1) (isValidClause assignment (first delta)))
    (t (AND (isValidClause assignment (first delta)) (isValidModel assignment (rest delta))))))

; ISINFERRED -- checks if any elements of clause is true, meaning that entire clause is inferred to be true.
(defun isInferred (assignment clause)
  (cond 
    ((not clause) NIL)
    (t (cond 
        ((isMember (first clause) assignment) t)
        (t (isInferred assignment (rest clause)))))))

; INFERDELTA -- parses delta and removes any clauses inferred to be true (using isInferred)
(defun inferDelta (assignment delta)
  (cond
    ((not delta) '())
    (t (cond
      ((isInferred assignment (first delta)) (inferDelta assignment (rest delta)))
      (t (cons (first delta) (inferDelta assignment (rest delta))))))))

; BACKTRACK -- if delta is satisfiable, return a list of n integers that represents a model
;   assignment of delta. Otherwise return NIL. 
;   Based on main algorithm for CSP - Backtracking DFS from 1B discussion slides.
(defun backtrack (assignment n delta)
  (cond
    ; if assignment is complete, return assignment
    ((AND (listp assignment) (= (length assignment) n)) assignment)
    (t 
      ; select next variable to assign 
      (let* ((var (if (not assignment) 1 (+ (abs (first assignment)) 1)))
            (varTrueAsmt (cons var assignment)) 
            (varFalseAsmt (cons (- var) assignment)))
        (cond
          ; if model with var=true is valid, recurse with updated assignment and inferred delta 
          ((isValidModel varTrueAsmt delta)
            (let* ((trueAssignment (backtrack varTrueAsmt n (inferDelta varTrueAsmt delta))))
              (cond 
                ((null trueAssignment) 
                  (cond 
                    ; if var=true didn't result in model, try var=false
                    ((isValidModel varFalseAsmt delta)
                      (let* ((falseAssignment (backtrack varFalseAsmt n (inferDelta varFalseAsmt delta))))
                        (cond
                          ((null falseAssignment) NIL)
                          (t falseAssignment))))
                    ; neither var=true nor var=false works, return NIL
                    (t NIL)))
                (t trueAssignment))))
          ; if model with var=false is valid, recurse with updated assignment and inferred delta 
          ((isValidModel varFalseAsmt delta)
            (let* ((falseAssignment (backtrack varFalseAsmt n (inferDelta varFalseAsmt delta))))
              (cond 
                ((null falseAssignment) NIL)
                (t falseAssignment))))
          ; neither var=true nor var=false works, return NIL
          (t NIL))))))

; SAT? -- finds model for delta using backtrack and initially empty assignment
(defun sat? (n delta)
  (backtrack '() n delta))

;(print (sat? 3 '((1 -2 3) (-1) (-2 -3))))
;(print (sat? 1 '((1) (-1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

;(format t "~%SAT F1: ~S~%" (solve-cnf "./cnfs/f1/sat_f1.cnf"))
;(format t "SAT F2: ~S~%" (solve-cnf "./cnfs/f2/sat_f2.cnf"))
;(format t "SAT F3: ~S~%" (solve-cnf "./cnfs/f3/sat_f3.cnf"))
;(format t "SAT F4: ~S~%" (solve-cnf "./cnfs/f4/sat_f4.cnf"))
;(format t "SAT F5: ~S~%" (solve-cnf "./cnfs/f5/sat_f5.cnf"))