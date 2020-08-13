; Katherine Miao

; BFS: takes TREE and returns a top-level list of the terminal nodes in the 
;       order they would be visited by a left-to-right breadth-first search
; Implementation: recurse thru tree, if at leaf node then just cons and BFS 
;       remaining branches, if branch then BFS appending this branch to the end
(defun BFS(TREE)
    (cond 
        ((not TREE) '())
        ((atom TREE) (list TREE))
        (t (if (atom (first TREE))
            (cons (first TREE) (BFS (rest TREE)))
            (BFS (append (rest TREE) (first TREE)))))))
;(print (bfs '((A (B)) C (D))))

; DFS: takes TREE and returns a top-level list of the terminal nodes in the 
;       order they would be visitied by a right-to-left depth-first search
; Implementation: recursively append the rest of this branch to the front of 
;       other branches to explore, call DFS on these SEPARATELY then append
(defun DFS(TREE)
    (cond
        ((not TREE) '())
        ((atom TREE) (list TREE))
        (t (append (DFS (rest TREE)) (DFS (first TREE))))))
;(print (dfs '((A (B)) C (D))))

; DLS (depth limited search): takes TREE and a maximum depth, returns a top-level 
;       list of the terminal nodes in the order they would be visitied by a 
;       left-to-right depth-first search with the limited depth
; Implementation: essentially DFS, but cutoff if depth is maxed out, otherwise 
;       recursively call DLS on next level and decrement depth
(defun DLS(TREE DEPTH)
    (cond
        ((< DEPTH 0) '())
        ((not TREE) '())
        ((atom TREE) (list TREE))
        (t (append (DLS (first TREE) (- DEPTH 1)) (DLS (rest TREE) DEPTH)))))

; DFID: takes TREE and the maximum depth, and returns a single top-level list of the 
;       terminal nodes in the order they would be visited by a left-to-right 
;       depth-first iterative-deepening search.
; Implementation: call DLS on all depths (increasing until reaching max depth) by
;       recursively appending DFID AND DLS
(defun DFID (TREE MAX)
    (cond
        ((>= MAX 0) (append (DFID TREE (- MAX 1)) (DLS TREE MAX)))
        (t '())))
;(print (DFID '((A (B)) C (D)) 3))
;(print (DFID '(A (B C) (D) (E (F G))) 3))

;====================================================================================
;====================================================================================
;====================================================================================

; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (if (equal s '(3 3 NIL))
    t
    NIL))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
; Implementation: check if action is valid or results in eaten missionaries, if not return state
(defun next-state (s m c)
  (let* (
    (currentM (first s))        
    (currentC (second s))       
    (currentSide (third s))
    (newM (+ (- 3 currentM) m))     ; missionaries on boat's side of river after action
    (newC (+ (- 3 currentC) c)))    ; cannibals on boat's side of river after action
    (cond
      ((OR (> m currentM) (> c currentC)) NIL)          ; check valid M/Cs to move
      ((> 1 (+ m c)) NIL)                               ; check at least 1 M/C
      ((AND (> newC newM) (> newM 0)) NIL)              ; check both riversides for too many C's
      ((AND (> (- currentC c) (- currentM m)) (> (- currentM m) 0)) NIL)
      (t (cons (list newM newC (not currentSide)) '())))))      ; all clear, return state

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
; Implementation: run through all possible actions, append if next-state is not NIL 
(defun succ-fn (s)
    (append (OR (next-state s 1 0) '()) (OR (next-state s 0 1) '()) (OR (next-state s 1 1) '())
          (OR (next-state s 2 0) '()) (OR (next-state s 0 2) '())))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
; Implementation: iterate through states and check for equality
(defun on-path (s states)
  (cond
    ((not states) NIL)
    ((equal s (first states)) t)
    (t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
; Implementation: iterate thru stack, apply mc-dfs to state 
;       then mult-dfs if no valid path exists
(defun mult-dfs (states path)
  ;(print states)
  ;(print path)
  (let (
    (currentState (first states)) 
    (restStates (rest states)))
    (cond
      ((not states) NIL)
      ((not (mc-dfs currentState path)) (mult-dfs restStates path))
      (t (mc-dfs currentState path)))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
; Implementation: call mult-dfs if not final state and state hasn't already occured
(defun mc-dfs (s path)
  ;(print s)
  ;(print path)
  (let (
    (nextPath (append path (list s)))
    ;(nextPath (append (list s) path))
    (nextS (succ-fn s)))
    ;(print "====")
    ;(print nextPath1)
    ;(print nextPath)
    (cond 
      ((final-state s) nextPath)
      ((on-path s path) NIL)
      (t (mult-dfs nextS nextPath)))))

;(print (next-state '(3 3 t) 1 0))    ; -> NIL
;(print (next-state '(3 3 t) 0 1))    ; -> ((0 1 NIL))
;(print (succ-fn '(3 3 t)))           ; -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
;(print (succ-fn '(1 1 t)))           ; -> ((3 2 NIL) (3 3 NIL))

(print (mc-dfs '(3 3 t) NIL))

