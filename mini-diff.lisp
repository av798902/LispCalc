;;------------------------------------------------------------------
;; MODIFIED AND PARTIAL COPY OF COMMON LISP PROJECT FOR CSC 345 -- Fall 2013
;; (Last Modified: (RWW) Sept 20, 2013)
;;
;; ALMOST ALL DOCUMENTATION HAS BEEN REMOVED
;;
;;===================================================================


(defun differentiate (F V) 
  (cond ((variable-p F)(if (equal (make-variable F)(make-variable V))
			   1    ; perhaps modify this using make-constant
			   0))  ; perhaps modify this using make-constant
	
	((sum-p F)(make-sum(differentiate(sum-operand-1 F) V)
			   (differentiate(sum-operand-2 F) V)))
	)
  )

;;===================================================================
;; (GLOBAL) SYMBOLS

(defconstant *variable-symbols*       '(U V W X Y Z))
(defconstant *sum-symbol*             '+)

;;===================================================================
;; READERS
;;---------------------------------------------
;; OPERATORS

(defun sum-operator (F) (second F))

;;---------------------------------------------
;; OPERANDS  

(defun sum-operand-1 (F)(first F))
(defun sum-operand-2 (F) (third F))

;;===================================================================
;; INQUISITORS


(defun variable-p (F) 
  (member F *variable-symbols*))


(defun sum-p (F)
  (and (listp F)
       (>= (length F) 3)
       (equal (sum-operator F) *sum-symbol*)))


;;===================================================================
;; CONSTRUCTORS


(defun make-variable (V) V)

(defun make-sum (F G)
  (cond ((eq 0 F) G)
	((eq 0 G) F)
	((and(numberp F)(numberp G))(+ F G))
	( t (list F *sum-symbol* G))))

  
;;===================================================================
;; The below is for personal testing only.  Once the program runs and
;; is tested to programmer's satisfaction, it would be removed.  It is
;; included here as an illustration only.
;; 

(defun nice-diff (F V)
  (format t "FUNCTION: ~a~%VARIABLE: ~a~%  RESULT: ~A~%~%" F V (differentiate F V)))

(defun t1 () (nice-diff 'x 'x))
(defun t2 () (nice-diff '(x + x) 'x))
(defun t3 () (nice-diff '(x + (x + x)) 'x))
(defun t4 () (nice-diff '((x + x) + (x + x)) 'x))

(defun testall ()
  (t1)
  (t2)
  (t3)
  (t4))

;;;-------------------------------------------------------------------
;;; SIMPLIFIED RUN  -- USES A SIMPLER FUNCTION THAN THE ONE FOR THE FULL PROJECT

;; * (testall)
;; FUNCTION: X
;; VARIABLE: X
;;   RESULT: 1

;; FUNCTION: (X + X)
;; VARIABLE: X
;;   RESULT: 2

;; FUNCTION: (X + (X + X))
;; VARIABLE: X
;;   RESULT: 3

;; FUNCTION: ((X + X) + (X + X))
;; VARIABLE: X
;;   RESULT: 4

;; NIL
;; * 
;;;-------------------------------------------------------------------
;;; END
