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

;If set to t, stdout/stderr for all tests (pass and fail) will be printed.
;If set to nil, only stdout/stderr for failed tests will be printed.
(defvar *utf-verbose* nil)

;chains all of the test names (name in deftest) that have ever been defined
(defvar *all-tests* nil)

;keeps track of the number of check calls (tests) that have run for each test function
(defvar *check-count* 0)

(defmacro! build-capture (outputs fstr success &body body)
  "captures the value of all output streams specified in outputs after evaluating body"
  (if outputs
    `(with-output-to-string (,(car outputs) ,fstr)
       (build-capture ,(cdr outputs) ,fstr ,success ,@body))
    `(progn
       ,@body
       (setf ,success t))))

(defmacro! capture-outputs (reprint outputs &body body)
  "captures and reprints (if specified) all output streams in outputs after evaling body"
  `(let ((,g!fstr (make-array '(0) :element-type 'base-char :fill-pointer 0 :adjustable t))
         (,g!success nil))
     (unwind-protect (build-capture ,outputs ,g!fstr ,g!success ,@body)
       (if (or ,reprint (not ,g!success))
         (format t "~a~%" ,g!fstr)))
     ,g!fstr))

(defmacro! capture-standard-output (reprint &body body)
  "captures and reprints (if specified) *standard-output* after evaluating body"
  `(capture-outputs ,reprint (*standard-output*) ,@body))

(defmacro! capture-error-output (reprint &body body)
  "captures and reprints (if specified) *error-output* after evaluating body"
  `(capture-outputs ,reprint (*error-output*) ,@body))

(defmacro! capture-output (reprint &body body)
  "captures and reprints (if specified) stdout/stderr after evaluating body"
  `(capture-outputs ,reprint (*standard-output* *error-output*) ,@body))

(defmacro! with-shadow ((fname fun) &body body)
  "shadow the function named fname with fun; any call to fname within body will use fun, instead of the default function for fname"
  `(let ((,g!res)
         (fun-orig))
     (cond ((fboundp ',fname) ;if there is already a function with that name defined, then shadow it
            (setf fun-orig (symbol-function ',fname))
            (setf (symbol-function ',fname) ,fun)
            (setf ,g!res (progn ,@body))
            (setf (symbol-function ',fname) fun-orig)
            (values))
           (t ;otherwise, define a new function with that name, and then undo the operation afterwards by unbinding that function
            (setf fun-orig #'identity)
            (setf (symbol-function ',fname) ,fun)
            (setf ,g!res (progn ,@body))
            (fmakunbound ',fname)
            (values)))
     ,g!res))

(defmacro with-shadows (shadows &body body)
  "Used to shadow multiple functions; each element in shadows is a function name / lambda pair that is passed to with-shadow"
  (if shadows
    `(with-shadow ,(car shadows)
       (with-shadows ,(cdr shadows) ,@body))
    `(progn ,@body)))

(defmacro errors-p (form &key (on-error `(format t "error: ~a~%" condition)))
  `(handler-case
     (progn ,form nil)
     (error (condition)
            ,on-error
            condition)))

(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call other test functions or use 'check' to run individual test cases."
  (multiple-value-bind (forms decls doc)
    #+:SBCL (sb-int:parse-body body)
    #-:SBCL (values body nil nil)
    `(progn
       (defun ,name ,parameters
         ,doc
         ,@decls
         (let ((*test-name* (append *test-name* (list ',name)))
               (*success* t)
               (*check-count* 0)
               (str))
           (setf str (capture-output nil ,@forms))
           (format t "Test: ~a, Num-Tests: ~a, Success: ~a~%" ',name *check-count* *success*)
           (when (or (not *success*) *utf-verbose*)
             (format t "Stdout/Stderr for test:~%~a~%" str))
           *success*))
       (push-to-end ',name *all-tests*))))

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
     (incf *check-count*)
     ,g!result))

(defmacro runtests (&body tests)
  "Top-level macro called to run a suite of tests
  With this macro, you explicitly define each test that you want to run"
  `(let ((*success* t))
     (combine-results ,@tests)
     *success*))

(defun run-all-tests ()
  "Top-level function called to run a suite of tests
  Runs all tests that have been defined using deftest"
  (notany #'null (mapcar #'funcall *all-tests*)))

(defun test-suite ()
  "Unit tests for the lisp code; will return t only if all unit tests pass; nil otherwise.
  All deftest-mm defined tests are collected in *all-tests*, and all of these will be run here.
  This means that any lisp code with unit tests that is loaded before calling test-suite, will
  have their tets registered in *all-tests*, and will be run when test-suite is called"
  (format t "~%overall: ~:[FAIL~;pass~]~%" (run-all-tests)))
