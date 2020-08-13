;
; CS161 Hw3: Sokoban
; Katherine Miao 
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions

; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
(defun reload()
  (load "hw3.lsp")
  )

; For loading a-star.lsp.
(defun load-a-star()
  (load "a-star.lsp"))

; Reloads hw3.lsp and a-star.lsp
(defun reload-all()
  (reload)
  (load-a-star)
  )

; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

; Helper function of getKeeperPosition
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; ===================================================================
; ====================   GOAL-TEST   ================================
; ===================================================================

; GOAL-TEST: returne true (t) if and only if s is a goal state of the game.
; 	(neither any boxes nor the keeper is on a non-goal square)
(defun goal-test (s)
	(cond 
		((not s) t)
		((atom s) (cond
			((isBox s) NIL)
			((isKeeper s) NIL)
			(t t)))
		(t (AND (goal-test (car s)) (goal-test (cdr s))))))

; ===================================================================
; ==============   GETSQUARE & SETSQUARE   ==========================
; ===================================================================

; GETELEMENT: helper for getSquare (below). Given some list of elements (all), 
; 	return the element at the specified index. Used to get a row from a state,
; 	and a square from a row. 

(defun getElement (all index)
	(cond
		((OR (not all) (< index 0)) NIL)
		; reached row depth! return this row
		((= index 0) (car all))
		; decrement row depth 
		(t (getElement (cdr all) (- index 1)))))

; GETSQUARE: takes in a State S, a row number r, and a column number c. 
; 	Returns the integer content of state S at square (r,c). 

(defun getSquare (s r c)
	(let* (
		(row (getElement s r))
		(square (getElement row c)))
		(cond
			; if the square is outside the scope, return the value of a wall
			((not square) wall)
			(t square))))

; SETELEMENT: helper for setSQUARE (below). Takes a row, column number, and value.
; 	Returns the row with the column replaced by val.

(defun setElement (r c val)
	(cond
		((not r) NIL)
		((= c 0) (cons val (cdr r)))
		(t (cons (car r) (setElement (cdr r) (- c 1) val)))))

; SETSQUARE: takes in a state S, row r, column c, and value val (integer)
; 	Returns a new state S’ that is obtained by setting square (r,c) to value v.

(defun setSquare (s r c val)
	(cond 
		((not s) NIL)
		((= r 0) (cons (setElement (car s) c val) (cdr s)))
		(t (cons (car s) (setSquare (cdr s) (- r 1) c val)))))

; ===================================================================
; =====================   MOVES   ===================================
; ===================================================================

; VALIDMOVE: return T if the values of the 1st and 2nd squares to be 
; 	affected by the move allow for the keeper to do so

(defun validMove (firstSquare secondSquare)
	(cond 
		; can't move if 1st square is wall
		((isWall firstSquare) NIL)
		; can't move if 1st square is box but 2nd square isn't blank or a star
		((isBox firstSquare) (OR (isBlank secondSquare) (isStar secondSquare)))
		(t T)))

; MOVE: takes a state and the coordinates for the keeper (r c), and the coordinates of
; 	the first and second squares affected by the move.
; 	(Example: if the keeper is at (r=0 c=0) and moves to the right, square1 is (r=0 c=1)
; 	and square2 is (r=0 c=2))

(defun move (s r c r1 c1 r2 c2)
	(let ((k (getSquare s r c))
		(sq1 (getSquare s r1 c1))
		(sq2 (getSquare s r2 c2)))
		(cond

		; MOVE TYPE: keeper to "empty" spot (only changes k + sq1)
			; keeper to blank
			((AND (isKeeper k) (isBlank sq1))
				(setSquare (setSquare s r1 c1 keeper) r c blank))
			; keeper to star
			((AND (isKeeper k) (isStar sq1))
				(setSquare (setSquare s r1 c1 keeperstar) r c blank))
			; keeperstar to blank
			((AND (isKeeperStar k) (isBlank sq1))
				(setSquare (setSquare s r1 c1 keeper) r c star))
			; keeperstar to star
			((AND (isKeeperStar k) (isStar sq1))
				(setSquare (setSquare s r1 c1 keeperstar) r c star))

		; MOVE TYPE: keeper to block spot (changes k + sq1 + sq2)
			; blank sq2
			((AND (isKeeper k) (isBox sq1) (isBlank sq2))
				(setSquare (setSquare (setSquare s r2 c2 box) r1 c1 keeper) r c blank))
			((AND (isKeeperStar k) (isBox sq1) (isBlank sq2))
				(setSquare (setSquare (setSquare s r2 c2 box) r1 c1 keeper) r c star))
			; star sq2
			((AND (isKeeper k) (isBox sq1) (isStar sq2))
				(setSquare (setSquare (setSquare s r2 c2 boxstar) r1 c1 keeper) r c blank))
			((AND (isKeeperStar k) (isBox sq1) (isStar sq2))
				(setSquare (setSquare (setSquare s r2 c2 boxstar) r1 c1 keeper) r c star))
			
			; blockstar, blank sq2
			((AND (isKeeper k) (isBoxStar sq1) (isBlank sq2))
				(setSquare (setSquare (setSquare s r2 c2 box) r1 c1 keeperstar) r c blank))
			((AND (isKeeperStar k) (isBoxStar sq1) (isBlank sq2))
				(setSquare (setSquare (setSquare s r2 c2 box) r1 c1 keeperstar) r c star))
			; blockstar, star sq2
			((AND (isKeeper k) (isBoxStar sq1) (isStar sq2))
				(setSquare (setSquare (setSquare s r2 c2 boxstar) r1 c1 keeperstar) r c blank))
			((AND (isKeeperStar k) (isBoxStar sq1) (isStar sq2))
				(setSquare (setSquare (setSquare s r2 c2 boxstar) r1 c1 keeperstar) r c star)))))

; TRYMOVE: takes a state, the coordinates of the keeper, and the direction it wants to move in. 
; 	Generates the coordinates for the first and second squares in that direction, then feeds into
; 	validMove. If the move is valid, do it by calling move. 
(defun tryMove (s r c dir)
	(cond 
		; the following branches do the same thing for the 4 directions:
		; 	check if the move is valid, and if so execute the move
		((equal dir 'RIGHT)
			; given the starting position and direction to move, 
			; 	generate the 1st and 2nd squares, then feed into validMove
			(let ((firstSquare (getSquare s r (+ c 1)))
				(secondSquare (getSquare s r (+ c 2))))
				(if (validMove firstSquare secondSquare)
					(move s r c r (+ c 1) r (+ c 2)))))
		((equal dir 'LEFT)
			(let ((firstSquare (getSquare s r (- c 1)))
				(secondSquare (getSquare s r (- c 2))))
				(if (validMove firstSquare secondSquare)
					(move s r c r (- c 1) r (- c 2)))))
		((equal dir 'UP)
			(let ((firstSquare (getSquare s (- r 1) c))
				(secondSquare (getSquare s (- r 2) c)))
				(if (validMove firstSquare secondSquare)
					(move s r c (- r 1) c (- r 2) c))))
		((equal dir 'DOWN)
			(let ((firstSquare (getSquare s (+ r 1) c))
				(secondSquare (getSquare s (+ r 2) c)))
				(if (validMove firstSquare secondSquare)
					(move s r c (+ r 1) c (+ r 2) c))))))

; NEXT-STATES: takes a state and returns the list of all states that can be reached 
; 	from the given state in one move
(defun next-states (s)
  	(let* ((pos (getKeeperPosition s 0))
		(c (car pos))
		(r (cadr pos))
		;x and y are now the coordinate of the keeper in s.
		(result (list (tryMove s r c'RIGHT) (tryMove s r c 'LEFT)
			(tryMove s r c 'UP) (tryMove s r c 'DOWN))))
    	(cleanUpList result)))

; ===================================================================
; ==================   HEURISTICS   =================================
; ===================================================================

; H0: trivial admissible heuristic. returns 0 for all states
(defun h0 (s)
	0)

; H1: returns # boxes not on a goal state in the given state
; 	This heuristic is admissable because for every box NOT on a goal state, 
; 	you need to nake AT LEAST one move. In the best case, you only need to
; 	make one move for each un-goaled box, therefore by adding 1 for every
; 	un-goaled box we're not overestimating. 
(defun h1 (s)
	(cond 
		((not s) 0)
		((AND (atom s) (isBox s)) 1)
		((atom s) 0)
		(t (+ (h1 (car s)) (h1 (cdr s))))))

; ISCORNERED: takes a state, row and column. Returns t if the given square is in a corner 
; 	formed by walls (thereby rendering it unmovable if it's a box)
(defun isCornered (s r c)
	(let ((rightWall (getSquare s r (+ c 1)))
		(leftWall (getSquare s r (- c 1)))
		(upWall (getSquare s (- r 1) c))
		(downWall (getSquare s (+ r 1) c)))
		(cond
			; check possible corner orientations 
			((AND (isWall leftWall) (isWall upWall)) t)
			((AND (isWall leftWall) (isWall downWall)) t)
			((AND (isWall rightWall) (isWall upWall)) t)
			((AND (isWall rightWall) (isWall downWall)) t)
			(t NIL))))

; HASCORNEREDBOXES: takes a state and a list of boxes' coordinates. Checks if any of 
; 	the boxes are in corners using isCornered
(defun hasCorneredBoxes (s boxes)
	(cond
		((not boxes) NIL)
		; check if this is a box's coordinates (list of length 2, no nested lists)
		((AND (listp boxes) (= (length boxes) 2) (not (listp (car boxes)))) 
			(cond
			((isCornered s (car boxes) (cadr boxes)) t)
			(t NIL)))
		(t (cond
			((hasCorneredBoxes s (car boxes)) t) 
			(t (hasCorneredBoxes s (cdr boxes)))))))

; GETBOXESFROMROW: given a row, its row number, and a column count, return a list of
; 	(row, column) coordinates in this row
(defun getBoxesFromRow (row r c)
	(cond 
		((not row) '())
		((isBox (car row))
			(cons (list r c) (getBoxesFromRow (cdr row) r (+ 1 c))))
		(t (getBoxesFromRow (cdr row) r (+ 1 c)))))

; GETBOXES: given a state and a row count, use getBoxesFromRow and return a list of
; 	(row, column) coordinates in this state
(defun getBoxes (s r)
	(cond 
		((null s) '())
		(t (append (getBoxesFromRow (car s) r 0) (getBoxes (cdr s) (+ r 1))))))

; HCORNEREDBOXES: a heuristic that scores a state unsolvable (h = 2500) if it has a box in a 
; 	walled corner,rendering it incapable of movement towards a star. 
; 	All other states with un-cornered boxes are scored with h = 0.  
(defun hCorneredBoxes (s)
	(let ((boxes (getBoxes s 0)))
		(cond 
			((not boxes) 0)
			((= (length boxes) 0) 0)
			(t (cond
				((hasCorneredBoxes s boxes) 2500)
				(t 0))))))

; H204970866: combines my heuristic involving cornered boxes (see above) and h1
(defun h204970866 (s)
	(+ (hCorneredBoxes s) (h1 s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
; GODHUID: (0:22)
; Nodes Expanded by A*: 1256
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(?)
; GODHUID (158 sec)
; Nodes Expanded by A*: 12455
; Solution depth: 51
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(?) Solution depth: 41
; GODHUID (204 seconds)
; Nodes Expanded by A*: 10977
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(?)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(?)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun

(setq s4 '((1 1 1 1 1)
 (1 0 2 4 1)
 (1 0 0 0 1)
 (1 0 0 0 1)
 (1 0 5 3 1)
 (1 1 1 1 1)
 )) 
 ;(print(next-states s4))