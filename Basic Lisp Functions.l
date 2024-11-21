(defun MY-LENGTH (L)
	(cond
		((null L) 0) ;; If L is empty, return 0
		(t (+ 1 (MY-LENGTH (cdr L)))) ;; Else, add 1 and recurse on rest of list
	)
)

(defun MY-MEMQ (A L)
	(cond
		((null L) NIL) ;; If L is empty, return NIL
		((eq A (car L)) L) ;; If A is first element, return rest of list starting at element matching A
		(t (MY-MEMQ A (cdr L))) ;; Else, recurse on rest of list
	)
)

(defun MY-APPEND (L1 L2)
	(cond
		((null L1) L2) ;; If L1 is empty, return L2
		(t (cons (car L1) (MY-APPEND (cdr L1) L2))) ;; Else, construct list and recursively append L1 and L2
	)
)

(defun MY-REVERSE-HELPER (L R)
	(cond
		((null L) R)
		(t (MY-REVERSE-HELPER (cdr L) (cons (car L) R)))
	)
)

(defun MY-REVERSE (L)
	(MY-REVERSE-HELPER L nil)
)

(defun MY-MAPCAR (F L)
	(cond
		((null L) NIL) ;; If L is empty, return NIL
		(t (cons (apply F (cons (car L) NIL)) (MY-MAPCAR F (cdr L)))) ;; Else, construct list, apply function F to first element, and recurse
	)
)

(defun MY-COPY (L)
	(cond
		((null L) NIL) ;; If L is empty, return NIL
		((atom L) L) ;; If L is an atom, return L
		(t (cons (MY-COPY (car L)) (MY-COPY (cdr L)))) ;; Else, recursively copy first element and recursively copy rest of list
	)
)

(defun MY-NTH (L N)
	(cond
		((= N 0) (cons NIL L)) ;; If N is 0, return (NIL . L)
		((null L) NIL) ;; If L is NIL, return NIL
		((= N 1) L) ;; If N is 1, return L
		(t (MY-NTH (cdr L) (1- N))) ;; Recur on rest of list with decremented N
	)
)

(defun MY-REMOVE (X L)
	(cond
		((null L) NIL) ;; If L is empty, return NIL
		((equal (car L) X) (MY-REMOVE X (cdr L))) ;; If first element is equal to X, recur on rest of list
		(t (cons (car L) (MY-REMOVE X (cdr L)))) ;; Else, include first element and recur on rest of list
	)
)

(defun MY-SUBST (X Y Z)
	(cond
		((null Z) NIL) ;; If Z is null, return NIL
		((and (atom Z) (eq Z X)) Y) ;; If Z is atom and Z is X, return Y
		((atom Z) Z) ;; If Z is atom and is not X, return Z
		(t (cons (MY-SUBST X Y (car Z)) (MY-SUBST X Y (cdr Z)))) ;; Else, include first element of Z and recur on rest of list
	)
)

(defun MY-SUBSET (FN L)
	(cond
		((null L) NIL) ;; If L is empty, return NIL
		((eq FN 'atom) ;; Check if FN is atom
			(cond
				((atom (car L)) (cons (car L) (MY-SUBSET FN (cdr L)))) ;; Include atom and recurse
				(t (MY-SUBSET FN (cdr L))) ;; Else, recurse on rest of list
			)
		)
		((eq FN 'listp) ;; Check if FN is listp
			(cond
				((listp (car L)) 
					(cons (car L) (MY-SUBSET FN (cdr L))) ;; Include list and recurse
				)
				(t (MY-SUBSET FN (cdr L))) ;; Else, recurse on rest of list
			)
		)
		(t (MY-SUBSET FN (cdr L))) ;; In all other cases, recurse on rest of list
	)
)

(defun MY-ADD-HELPER (N1 N2 CARRY)
	(cond
		((and (null N1) (null N2) (= CARRY 0)) NIL) ;; If N1 and N2 are empty and no carry left, return NIL
    		((and (null N1) (null N2)) (cons CARRY NIL)) ;; If N1 and N2 are empty but carry left, return list with just carry
		(t
			(cons
				(mod (+ (cond ((null N1) 0) (t (car N1))) ;; Create new list by adding current digit, adding last digit of sum, summing current digits of N1 and N2 and carry
				(cond ((null N2) 0) (t (car N2))) CARRY) 10) ;; Get last digit with sum modulo 10
				(MY-ADD-HELPER (cond ((null N1) NIL) (t (cdr N1))) (cond ((null N2) NIL) (t (cdr N2))) ;; Recurse on remaining digits
					(floor (+ (cond ((null N1) 0) (t (car N1))) ;; Calculate new carry for next recursive step
					(cond ((null N2) 0) (t (car N2))) CARRY) 10) ;; Get carry for next recursive step with floor division 10
				)
			)
		)
	)
)

(defun MY-ADD (N1 N2)
	(MY-ADD-HELPER N1 N2 0) ;; Call helper function to manage carry, carry starting at 0
)

(defun MY-MERGE (L1 L2)
	(cond
		((null L1) L2) ;; If L1 is empty, return L2
		((null L2) L1) ;; If L2 is empty, return L1
		((<= (car L1) (car L2)) (cons (car L1) (MY-MERGE (cdr L1) L2))) ;; If head of L1 <= head of L2, add head of L1 and merge rest of list
		(t (cons (car L2) (MY-MERGE L1 (cdr L2)))) ;; Else, add head of L2 and merge rest of list
	)
)

(defun MY-SUBLIST-CONTIGUITY (L1 L2)
	(cond
		((null L1) t) ;; If L1 is empty, return true (empty list is always a sublist)
		((null L2) NIL) ;; If L2 is empty, return false (L1 cannot be sublist of empty list)
		((eq (car L1) (car L2)) (MY-SUBLIST-CONTIGUITY (cdr L1) (cdr L2))) ;; Keeping track of contiguity of sublist
		(t NIL) ;; If sublist not contiguous, return NIL
	)
)

(defun MY-SUBLIST (L1 L2)
	(cond
		((null L1) t) ;; If L1 is empty, return true (empty list is always a sublist)
		((null L2) NIL) ;; If L2 is empty, return false (L1 cannot be sublist of empty list)
		((eq (car L1) (car L2)) (MY-SUBLIST-CONTIGUITY (cdr L1) (cdr L2))) ;; Keeping track of contiguity of sublist
		(t (MY-SUBLIST L1 (cdr L2))) ;; Else, check L1 with rest of L2 recursively
	)
)

(defun MY-ASSOC (A ALIST)
	(cond
		((null ALIST) NIL) ;; If ALIST is empty, return NIL
		((eq (car (car ALIST)) A) (car ALIST)) ;; If key in first pair is A, return the pair
		(t (MY-ASSOC A (cdr ALIST))) ;; Else, recurse on rest of ALIST
	)
)

(defun MY-REPLACE (ALIST L)
	(cond
		((atom L) ;; Check if L is atom
			(cond
				((MY-ASSOC L ALIST) (cdr (MY-ASSOC L ALIST))) ;; If L has corresponding pair, return corresponding value
				(t L) ;; Else, return L
			)
		)
		(t (cons (MY-REPLACE ALIST (car L)) (MY-REPLACE ALIST (cdr L)))) ;; If L is not atom, replace first element and recur on rest of list
	)
)