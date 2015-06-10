;;;Project 1

(defun integrate (F V &optional lo hi)
  "Definite integral used with optional parameters, else indefinite integral"
  ;;; Call one of two function
  (def-integral (indef-integral F V) V lo hi))


(defun indef-integral(F V)
  "Indefinite integral is calculated without optional parameters"
  ;;; Return product if we are passing numbers
  (cond	((number-p F) (make-product F V))
	;;; Return integral of power with variable
	((variable-p F) (integrate (make-power F 1) V))
	;;; Negative integral case
	((negative-p F) (make-negative (integrate (make-negative F) V)))
	;;; Add integrals of both sum operands
	((sum-p F) (make-sum 
		    (integrate (sum-operand-1 F) V) 
		    (integrate (sum-operand-2 F) V)))
	;;; Find difference between integrals of difference operands
	((difference-p F)(make-difference 
			 (integrate (difference-operand-1 F) V) 
			 (integrate (difference-operand-2 F) V)))
	;;; Logarithm case, we look for a negative 1 as an exponent
	((and (power-p F) (equalp (power-operand-2 F) -1))
	 (make-log (power-operand-1 F) V))
	;;; Standard power case where exponent is not negative 1
	((and (power-p F) (not (equalp (power-operand-2 F) -1)))
	 (make-quotient (make-power V (make-sum (power-operand-2 F) 1)) (make-sum(power-operand-2 F) 1)))))
       ;;; (t (variable-p F) (integrate (make-power F 1) V)))))

(defun def-integral(F V lo hi)
  "Definite integreal is calculated when given upper and lower limits"
 ;;; Return argument F is lo and hi aren't actually values we can use
 (cond ((not (and (number-p lo) (number-p hi))) F)
       ;;; Otherwise, evaluate the definite integral
       (t (eval (make-difference (subst hi V F) (subst lo V F))))))

;;;=========================================================
;;; SYMBOLS
;;; These symbols are used in later defuns and can be used for 
;;; running evaluation or to be included in a list with operands
;;; should the equation not be capable of being computed
(defconstant *variable-symbols* '(U V W X Y Z))
(defconstant *negative-symbol* '-)
(defconstant *sum-symbol* '+)
;;; Difference symbol is same as negative 
;;; Can we use negative-symbol later and eval?
(defconstant *difference-symbol* '-)
(defconstant *product-symbol* '*)
(defconstant *quotient-symbol* '/)
(defconstant *power-symbol* 'expt)
;;; While log is going to be our power when the exponent is -1
;;; having a log symbol can prove useful for evaluation or when
;;; forming a list when the arguments cannot be operated on
(defconstant *log-symbol* 'log)

;;;=========================================================
;;; SELECTORS -- OPERATORS
;;; All operators are the first item of the list in LISP
;;; Necessary Operators for our operands also in the list:
(defun negative-operator (F) (first F))  
(defun sum-operator (F) (first F))
(defun difference-operator (F) (first F))
(defun product-operator (F) (first F))
(defun quotient-operator (F) (first F))
(defun power-operator (F) (first F))
;;;log-operator
;;;We don't need a log operator, we'll just try using a conditional
;;;when the second operand for power is -1

;;; SELECTORS -- OPERANDS
;;; Negative only needs 1 operator & 1 operand while all other operands
;;; will be the second and third items of the list in LISP
(defun negative-operand (F)(second F))
;;; Sum operands 
(defun sum-operand-1 (F) (second F))
(defun sum-operand-2 (F) (third F))
;;; Difference Operands
(defun difference-operand-1 (F) (second F))
(defun difference-operand-2 (F) (third F))
;;; Product Operands
(defun product-operand-1 (F) (second F))
(defun product-operand-2 (F) (third F))
;;; Quotient Operands
(defun quotient-operand-1 (F) (second F))
(defun quotient-operand-2 (F) (third F))
;;; Power Operands
(defun power-operand-1 (F) (second F))
(defun power-operand-2 (F) (third F))
;;; Log Operand  
;;; No need for log operands, we'll be taking advantage of the power operands

;;;=========================================================
;;; PREDICATES
;;; All of thse will be used in our cond clause statement and act
;;; sort of like a switch to let us know what steps to take after we 
;;; pass arguments in
(defun variable-p (F)
  "Verifies that argument F is a variable"
  (member F *variable-symbols*))

(defun number-p (F)
  "Verifies that argument F is an actual number"
  (numberp F))

(defun negative-p (F)
  "Verifies that argument F is a number and negative"
  ;;; Can a list or atom be less than 0
  ;;; Do I need to bother checking if it is a number?
  (cond ((and (number-p F) (< F 0)) t)
	((variable-p F) nil)
	((equalp (first F) *negative-symbol*) t)))

;;; The logic that works for sum-p should also work with other function
(defun sum-p (F)
  "Verifies that we are dealing with the sum function"
  (cond ((number-p F) nil) ;;; We want a function, not a number
	((variable-p F) nil) ;;; We want a function, not a variable
	((and (equalp (sum-operator F) *sum-symbol*) 
	      (sum-operand-1 F) (sum-operand-2 F)) t))) ;;; Checks the list for proper operands and operator

(defun difference-p (F)
  "Verifies that we are dealing with the difference function"
  (cond ((number-p F) nil) ;;; We want a function, not a number
	((variable-p F) nil) ;;; We want a function, not a variable
	((and (equalp (difference-operator F) *difference-symbol*)
	      (difference-operand-1 F) (difference-operand-2 F)) t))) ;;; Checks the list for proper operands and operator
(defun product-p (F)
  "Verifies that we are using the product function"
  (cond ((number-p F) nil) ;;; We want a function, not a number
	((variable-p F) nil) ;;; We want a function, not a variable
	((and (equalp (product-operator F) *product-symbol*)
	      (product-operand-1 F) (product-operand-2 F)) t))) ;;; Checks the list for proper operands and operator

(defun quotient-p (F)
  "Verifies that we are using the quotient function"
  (cond ((number-p F) nil) ;;; We want a function, not a number
	((variable-p F) nil) ;;; We want a function, not a variable
	((and (equalp (quotient-operator F) *quotient-symbol*)
	      (quotient-operand-1 F) (quotient-operand-2 F)) t))) ;;; Checks the list for proper operands and operator

(defun power-p (F)
  "Verifies that we are using the power function"
  (cond ((number-p F) nil) ;;; We want a function, not a number
	((variable-p F) nil) ;;; We want a function, not a variable
	((and (equalp (power-operator F) *power-symbol*)
	      (power-operand-1 F) (power-operand-2 F)) t))) ;;; Checks the list for proper operands and operator

;;; This should be helpful in simplifying our multiple negatives 
;;; such as (--x) or (---x)
(defun multineg-p (F)
  "Verifies that argument F is a multiply negative number"
  ;;; Negative only has an operator and operand, more than length of 2
  ;;; must mean that we have multiple negative signs
  (cond ((> 2 (length F)) t)))

;;; Again, no need for log-p since this we will detect with exponents

;;;=========================================================
;;; CONSTRUCTORS
;;; make-variable is included for completeness of abstraction
(defun make-variable (V) V)

(defun make-negative(F)
  "Makes the argument F taken in negative"
  (cond ((number-p F) (* -1 F))
	((negative-p F) (negative-operand F)) ;;;(--x) not (-(-x))
	((multineg-p F) (make-compound-neg F))
        (t (list *negative-symbol* F)))
  (if (multineg-p F)(make-compound-neg F)))

(defun make-compound-neg (F)
  "Simplifies a case of multiple negatives"
  ;;; An even length means we must have an odd number of negative symbol
  ;;; so we return a negative number (a neg sign and last of the list)
 (cond ((equalp (mod (length F) 2) 0) (list *negative-symbol* (last F)))
	;;; The only other case is that we have even number of negative symbols 
	;;; which is the same as just the base number (the last of the list)
	(t (list (last F)))))

(defun make-sum (F G)
  "Returns the addition of arguments F and G"
  (cond ((equalp 0 F) G)
	((equalp 0 G) F)
	((and(number-p F)(number-p G))(+ F G))
	(t (list *sum-symbol* F G))))

(defun make-difference (F G)
  "Returns the difference of arguments F and G"
  (cond ((equalp F G) 0)
	((equalp 0 F) G)
	((equalp 0 G) F)
	((and (number-p F) (number-p G)) (- F G))
	(t (list *difference-symbol* F G))))

(defun make-product (F G)
  "Returns the product of arguments F and G"
  (cond ((equalp 0 F) 0)
	((equalp 0 G) 0)
	((equalp 1 F) G)
	((equalp 1 G) F)
	((equalp -1 F) (make-negative G) );;; We should try to take advantage of constructors we made
	((equalp -1 G) (make-negative F)) ;;; These two cases could be taken care of with normal multiply
	((and(number-p F) (number-p G)) (* F G))
	(t (list *product-symbol* F G))))

(defun make-quotient (F G)
  "Returns the quotient of arguments F and G; F is divided by G"
  (cond ((equalp 0 F) 0)
	((equalp 0 G) "Error; cannot divide by zero")
	((and(number-p F) (number-p G)) (/ F G))
	(t (list *quotient-symbol* F G))))

(defun make-power (F G)
  "Returns the value returned by raising base, F, by the exponent, G"
  (cond ((and(equalp 0 F) (equalp 0 G)) "Domain error; cannot raise zero by zero") 
	((equalp 0 F) 0)
	((equalp 0 G) 1)
	((and(number-p F) (number-p G)) (expt F G))
	(t (list *power-symbol* F G))))

;;; This log function will REQUIRE the log base as opposed to the built-in
;;; log function in LISP where the base is an optional parameter
(defun make-log (F G)
  "Returns the log of number, F, with the log base, G"
  (cond ((or (equalp 0 F) (equalp 0 G)) "Domain error; log base/number cannot be zero")
	((and (number-p F) (number-p G)) (log F G))
	(t (list *log-symbol* F G))))
