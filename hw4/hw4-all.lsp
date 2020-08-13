;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists

(defun isMember (item L)
  (cond 
    ((not (member item L)) NIL)
    (t t)))

(defun isValidClause (assignment clause)
  ;(print "isValidClause")
  ;(print clause)
  (cond 
    ((not clause) 
      ;(format t "clause is invalid~%") 
      NIL)
    (t (let* ((var (first clause)) (absvar (abs var)))
      (cond 
        ((OR 
             (AND (not (isMember var assignment)) (not (isMember (- var) assignment))) ;check this
             (isMember var assignment))
             ;(format t "valid clause ~S~%" clause) 
             t)
        (t (isValidClause assignment (rest clause))))))))
;(print (isValidClause '(-1 2 -3) '(1 -2 3)))

(defun isValidModel (assignment delta)
  ;(format t "~%--- isValidModel. assignment = ~S~%" assignment)
  (cond
    ((not delta) ;(print "--- model is valid") 
      t)
    ((= (length delta) 1) (isValidClause assignment (first delta)))
    (t (AND (isValidClause assignment (first delta)) (isValidModel assignment (rest delta))))))
;(print (isValidModel '(-1 -2) '((1 -2 3) (-1) (-2 -3))))

(defun getBacktrackedAssignment (assignment)
  (cond
    ((not assignment) '())
    ((> (car assignment) 0) (cons (- (car assignment)) (rest assignment)))
    (t (getBacktrackedAssignment (cdr assignment)))))

(defun oldBacktrack (assignment n delta)
  (format t "~%===================================BACKTRACK")
  (format t "~%===================================ASSIGNMENT: ~S~%" assignment)
  (cond
    ; if assignment is complete, return assignment
    ((AND (listp assignment) (= (length assignment) n)) assignment)
    (t 
      ; select a variable to assign 
      (let* ((var (if (not assignment) 1 (+ (abs (first assignment)) 1)))
            (varTrueAsmt (cons var assignment)) 
            (varFalseAsmt (cons (- var) assignment)))
        (format t "var: ~D~%" var)
        (cond
          ; try var=true
          ((isValidModel varTrueAsmt delta)
              (oldBacktrack varTrueAsmt n delta))
          ; try var=false
          ((isValidModel varFalseAsmt delta)
              (oldBacktrack varFalseAsmt n delta))
          ; backtrack!
          (t (format t "gotta backtrack!~%new assignment=~S~%" (getBacktrackedAssignment assignment)) 
            (cond 
              ((not assignment) NIL)
              (t (oldBacktrack (getBacktrackedAssignment assignment) n delta)))))))))
;(print (oldBacktrack '() 3 '((1 -2 3) (-1) (-2 -3))))

(defun isInferred (assignment clause)
  (cond 
    ((not clause) NIL)
    (t (cond 
        ((isMember (first clause) assignment) t)
        (t (isInferred assignment (rest clause)))))))

(defun inferDelta (assignment delta)
  (cond
    ((not delta) '())
    (t (cond
      ((isInferred assignment (first delta)) (inferDelta assignment (rest delta)))
      (t (cons (first delta) (inferDelta assignment (rest delta))))))))

(defun backtrack (assignment n delta)
  ;(format t "~%===================================BACKTRACK")
  ;(format t "~%===================================ASSIGNMENT: ~S" assignment)
  ;(format t "~%===================================DELTA: ~S~%" delta)
  (cond
    ; if assignment is complete, return assignment
    ((AND (listp assignment) (= (length assignment) n)) assignment)
    (t 
      ; select a variable to assign 
      (let* ((var (if (not assignment) 1 (+ (abs (first assignment)) 1)))
            (varTrueAsmt (cons var assignment)) 
            (varFalseAsmt (cons (- var) assignment)))
        (cond
          ; try var=true
          ((isValidModel varTrueAsmt delta) ;(format t "~%starting backtracking TRUE for var ~S" var)
            (let* ((trueAssignment (backtrack varTrueAsmt n (inferDelta varTrueAsmt delta))))
              ;(format t "~%done backtracking TRUE for var ~S, got assignment ~S" var trueAssignment)
              (cond 
                ((null trueAssignment) 
                  (cond 
                    ((isValidModel varFalseAsmt delta) ;(format t "~%starting NESTED backtracking FALSE for var ~S" var)
                      (let* ((falseAssignment (backtrack varFalseAsmt n (inferDelta varFalseAsmt delta))))
                        ;(format t "~%done backtracking FALSE for var ~S, got assignment ~S" var falseAssignment)
                        (cond
                          ((null falseAssignment) NIL)
                          (t falseAssignment))))
                    (t NIL)))
                (t trueAssignment))))
          ; try var=false
          ((isValidModel varFalseAsmt delta) ;(format t "~%starting backtracking FALSE for var ~S" var)
            (let* ((falseAssignment (backtrack varFalseAsmt n (inferDelta varFalseAsmt delta))))
              ;(format t "~%done backtracking FALSE for var ~S, got assignment ~S" var falseAssignment)
              (cond 
                ((null falseAssignment) NIL)
                (t falseAssignment))))
          (t NIL))))))

(defun sat? (n delta)
  (format t "~%===================================SAT?")
  (format t "~%===================================DELTA: ~S~%" delta)
  (format t "~%===================================N: ~S~%" n)
  (print (backtrack '() n delta)))

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

(solve-cnf "./cnfs/f1/sat_f1.cnf")