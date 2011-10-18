;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  -*- mode: LISP; Syntax: COMMON-LISP;  Base: 10 -*-
;;; 
;;; Author      : Clayton Stanley
;;; Address     : Air Force Research Laboratory
;;;             : Mesa, AZ 85212 USA
;;;             : clayton.stanley@wpafb.af.mil
;;; Filename    : unitTestFramework.lisp
;;; Version     : 1.0
;;; 
;;; Description : A Lisp-Based Unit Testing Framework. 
;;;               Most of the core was taken from gigamonkeys.com UTF tutorial
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
;chains all of the test names (name in deftest) that have been defined
;in the hierarchy; used when printing documentation for a 'check' test
(defvar *test-name* nil)

;keeps track of the success for a test; if a hierarchy of tests are set up,
;when a test lower down in the hierarchy fails, the failure is propogated
;all the way up
(defvar *success* t)

(defmacro! with-shadow ((fname fun) &body body)
  "shadow the function named fname with fun; any call to fname within body will use fun, instead of the default function for fname"
  (cond ((fboundp fname) ;if there is already a function with that name defined, then shadow it
	 `(let ((fun-orig (symbol-function ',fname)))
	    (setf (symbol-function ',fname) ,fun)
	    ,@body
	    (setf (symbol-function ',fname) fun-orig)
	    nil))
	(t ;otherwise, define a new function with that name, and then undo the operation afterwards by unbinding that function
	 `(progn
	    (setf (symbol-function ',fname) ,fun)
	    ,@body
	    (fmakunbound ',fname)
	    nil))))

(defmacro! errors-p (form)
  `(handler-case
       (progn
	 ,form
	 nil)
     (error (,g!condition) ,g!condition)))

(defmacro runtests (&body tests)
  "top-level macro called to run a suite of tests"
  `(let ((*success* t))
     (combine-results ,@tests)
     *success*))
 
(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test functions or use 'check' to run individual test cases."
  (multiple-value-bind (forms decls doc)
      #+:SBCL (sb-int:parse-body body)
      #-:SBCL (values body nil nil)
      `(defun ,name ,parameters
	 ,doc
	 ,@decls
	 (let ((*test-name* (append *test-name* (list ',name)))
	       (*success* t))
	   ,@forms
	   *success*))))

(defmacro check (&body forms)
  "Run each expression in 'forms' as a test case."
  `(combine-results
    ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro! combine-results (&body forms)
  "Combine the results (as booleans) of evaluating 'forms' in order."
  `(let ((,g!result t))
     ,@(loop for f in forms collect `(unless ,f (setf ,g!result nil)))
     (unless ,g!result (setf *success* nil))
     ,g!result))

(defmacro! report-result (o!result form)
  "Report the results of a single test case. Called by 'check'."
  `(progn
     (format t "~:[FAIL~;pass~] ... ~a: ~a~%" ,g!result *test-name* ,form)
     ,g!result))





