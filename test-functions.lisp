;;===================================================================
;; The below is for personal testing only.  Once the program runs and
;; is tested to programmer's satisfaction, it would be removed.  It is
;; included here as an illustration only.
;; 
;;;============================================================================
;;; functions to run the examples

(defun run-all-diffs (n)
  "Run differentiate on all functions from F1 to F<n>"
  (terpri)
  (run-all-diffs2 1 n))


(defun run-all-diffs2 (m n)
  "Helper function for RUN-DIFFS -- evaluate NICE-DIIF on F<m>--F<n>"
  (cond ((> m n) nil)
	(t (make-and-eval-differentiate-expression m)
	   (run-all-diffs2 (1+ m) n))))

(defun build-symbol (n)
  "Makes (and interns) the lisp symbol F<n>, for example: F1 or F2 or F3, etc. "
  (intern (concatenate 'string "F" (prin1-to-string n))))

(defun make-and-eval-differentiate-expression (n)
  "Build and evaluate the expression for differentiating F<n> w.r.t. variable x (or y if n=6)"
  (format t "DIFFERENTIATING FUNCTION F~a" n)
  (if (= n 6)(format t "               NB: differentiate here w.r.t. y, not x"))
  (terpri)
  (eval `(nice-diff ,(build-symbol n) (if (= ,n 6) 'y 'x))))

(defun nice-diff (F V)
  (format t "FUNCTION: ~a~%VARIABLE: ~a~%  RESULT: ~A~%~%" F V (differentiate F V)))

;;;===================================================================
;;;  THE TEST EXPRESSIONS

(defconstant f1 'a)
(defconstant f2 '(x / 3))
(defconstant f3 '(- x))
(defconstant f4 '(- - x))
(defconstant f5 '(- - - x))          ;; NB: F5 is to be differentiated wrt y, not x
(defconstant f6 '(x / 5))
(defconstant f7 '(x + x))
(defconstant f8 '(- - (x + x)))
(defconstant f9 '((x + x) + x))
(defconstant f10 '(x - (- x)))
(defconstant f11 '(4 * x))
(defconstant f12 '(x * x))
(defconstant f13 '((- x) + (- x)))
(defconstant f14 '(x / x))
(defconstant f15 '(x / (2 * x)))
(defconstant f16 '((x * x) * (x * x)))
(defconstant f17 '((4 * x) - (2 * x)))
(defconstant f18 '(3 * (x ** 4)))
(defconstant f19 '(a * (x ** 3)))
(defconstant f20 '(-2 * (x ** -3)))


;;;=============================================================================================
;;;  DEMONSTATION RUN

;; * (run-all-diffs 20)

;; DIFFERENTIATING FUNCTION F1
;; FUNCTION: A
;; VARIABLE: X
;;   RESULT: 0

;; DIFFERENTIATING FUNCTION F2
;; FUNCTION: (X / 3)
;; VARIABLE: X
;;   RESULT: 1/3

;; DIFFERENTIATING FUNCTION F3
;; FUNCTION: (- X)
;; VARIABLE: X
;;   RESULT: -1

;; DIFFERENTIATING FUNCTION F4
;; FUNCTION: (- - X)
;; VARIABLE: X
;;   RESULT: 1

;; DIFFERENTIATING FUNCTION F5
;; FUNCTION: (- - - X)
;; VARIABLE: X
;;   RESULT: -1

;; DIFFERENTIATING FUNCTION F6               NB: differentiate here w.r.t. y, not x
;; FUNCTION: (X / 5)
;; VARIABLE: Y
;;   RESULT: 0

;; DIFFERENTIATING FUNCTION F7
;; FUNCTION: (X + X)
;; VARIABLE: X
;;   RESULT: 2

;; DIFFERENTIATING FUNCTION F8
;; FUNCTION: (- - (X + X))
;; VARIABLE: X
;;   RESULT: 2

;; DIFFERENTIATING FUNCTION F9
;; FUNCTION: ((X + X) + X)
;; VARIABLE: X
;;   RESULT: 3

;; DIFFERENTIATING FUNCTION F10
;; FUNCTION: (X - (- X))
;; VARIABLE: X
;;   RESULT: 2

;; DIFFERENTIATING FUNCTION F11
;; FUNCTION: (4 * X)
;; VARIABLE: X
;;   RESULT: 4

;; DIFFERENTIATING FUNCTION F12
;; FUNCTION: (X * X)
;; VARIABLE: X
;;   RESULT: (X + X)

;; DIFFERENTIATING FUNCTION F13
;; FUNCTION: ((- X) + (- X))
;; VARIABLE: X
;;   RESULT: -2

;; DIFFERENTIATING FUNCTION F14
;; FUNCTION: (X / X)
;; VARIABLE: X
;;   RESULT: 0

;; DIFFERENTIATING FUNCTION F15
;; FUNCTION: (X / (2 * X))
;; VARIABLE: X
;;   RESULT: (((2 * X) - (X * 2)) / ((2 * X) * (2 * X)))

;; DIFFERENTIATING FUNCTION F16
;; FUNCTION: ((X * X) * (X * X))
;; VARIABLE: X
;;   RESULT: (((X * X) * (X + X)) + ((X * X) * (X + X)))

;; DIFFERENTIATING FUNCTION F17
;; FUNCTION: (X ** 4)
;; VARIABLE: X
;;   RESULT: (4 * (X ** 3))

;; DIFFERENTIATING FUNCTION F18
;; FUNCTION: (3 * (X ** 4))
;; VARIABLE: X
;;   RESULT: (3 * (4 * (X ** 3)))

;; DIFFERENTIATING FUNCTION F19
;; FUNCTION: (A * (X ** 3))
;; VARIABLE: X
;;   RESULT: (A * (3 * (X ** 2)))

;; DIFFERENTIATING FUNCTION F20
;; FUNCTION: (-2 * (X ** -3))
;; VARIABLE: X
;;   RESULT: (-2 * (-3 * (X ** -4)))

;; NIL
;; * 
;;;=============================================================================================
;;; END
 
