; Katherine Miao

; TREE-CONTAINS: checks whether number N appears in the ordered tree TREE
; recursively traverse TREE to reach all nodes and check for equality
(defun TREE-CONTAINS(N L)
    (cond 
        ((not L) NIL)
        ((atom L) (= N L))
        (t (OR (TREE-CONTAINS N (first L)) (TREE-CONTAINS N (rest L))))))

; TREE-MIN: returns the minimum number appearing in the ordered tree TREE
; keep calling "first" to get to first element of TREE 
(defun TREE-MIN(L)
    (cond
        ((not L) NIL)
        ((atom L) L)
        (t (TREE-MIN (first L)))))

; TREE-ORDER: returns a pre-ordered list of the numbers appearing in the ordered tree TREE
(defun TREE-ORDER(L)
    (cond
        ((not L) '())
        ((atom L) (list L))
        (t (cond 
            ; if no sublists, then no splitter. just merge left and right
            ((not (cadr L)) (append (TREE-ORDER (car L)) (TREE-ORDER (cddr L))))
            ; if sublists, splitter comes first, then left then right
            (t (cons (cadr L) (append (TREE-ORDER (car L)) (TREE-ORDER (cddr L)))))))))

; SUB-LIST: returns the sub-list of L starting at position START and having length LEN
; recurse while reaching zero for both START and LEN
(defun SUB-LIST(L START LEN)
    (cond
        ((not L) '())
        ((> START 0) (SUB-LIST (cdr L) (- START 1) LEN))
        (t (cond
            ((> LEN 0) (cons (car L) (SUB-LIST (cdr L) START (- LEN 1))))
            (t '())))))

; SPLIT-LIST: returns a list of two lists L1 and L2, in that order, such that
;               - L is the result of appending L1 and L2;
;               - Length of L1 minus length of L2 is 0 or 1.
(defun SPLIT-LIST(L)
    (let* ((len (length L))
        (halflen (/ len 2)))
            ; use SUB-LIST to get first and last half
            (list (SUB-LIST L 0 halflen) (SUB-LIST L halflen (- len halflen)))))

; BTREE-HEIGHT: returns the height of TREE
; use "max" to explore depths of all branches
(defun BTREE-HEIGHT(TREE)
    (cond
        ((not TREE) 0)
        ((atom TREE) 0)
        (t (max (+ 1 (BTREE-HEIGHT (car TREE))) (+ 1 (BTREE-HEIGHT (cadr TREE)))))))

; LIST2BTREE: takes a non-empty list of atoms LEAVES, and returns a binary tree such that
;       - The tree leaves are the elements of LEAVES;
;       - For any internal (non-leaf) node in the tree, the number of leaves in its 
;           left branch minus the number of leaves in its right branch is 0 or 1.
(defun LIST2BTREE(LEAVES)
    (cond
        ; base cases:
        ((not LEAVES) '())
        ((= (length LEAVES) 1) (car LEAVES))
        ((= (length LEAVES) 2) LEAVES)
        ; for non-base cases, recurse and build btrees for left and right halves:
        (t (let* (
            (splits (SPLIT-LIST LEAVES))
            (left (car splits))
            (right (cadr splits)))
            (list (LIST2BTREE left) (LIST2BTREE right))))))

; BTREE2LIST: returns a list of atoms from TREE (the inverse of LIST2BTREE)
; recursively construct list from left and right halves, then merge
(defun BTREE2LIST(TREE)
    (cond 
        ((not TREE) NIL)
        ((atom TREE) (list TREE))
        ((= (length TREE) 2) 
            (append (BTREE2LIST (car TREE)) (BTREE2LIST (cadr TREE))))))

; IS-SAME: takes two LISP expressions E1 and E2 whose atoms are all numbers
; and checks whether the expressions are identical
(defun IS-SAME(E1 E2)
    (cond
        ((AND (not E1) (not E2)) t)
        ((AND (atom E1) (atom E2)) (= E1 E2))
        ; NIL if list structure doesn't match
        ((AND (atom E1) (listp E2)) NIL)
        ((AND (listp E1) (atom E2)) NIL)
        ; recursively traverse branches 
        (t (AND (IS-SAME (car E1) (car E2)) (IS-SAME (cdr E1) (cdr E2))))))