(defvar *test-name* nil)
(defvar *success* t)

(defmacro runtests (&body tests)
  `(let ((*success* t))
     (combine-results ,@tests)
     *success*))
 
(defmacro deftest (name parameters &body body)
  "Define a test function. Within a test function we can call
   other test functions or use 'check' to run individual test
   cases."
  (let ((doc))
    (when (and (> (length body) 1) (stringp (car body)))
      (setf doc (car body))
      (setf body (cdr body)))
    `(defun ,name ,parameters
       ,doc
       (let ((*test-name* (append *test-name* (list ',name)))
	     (*success* t))
	 ,@body
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





