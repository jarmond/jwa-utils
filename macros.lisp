;;;; Convenience macros for enhancing Lisp

(defmacro o (&body body)
  `(compose ,@body))

(defmacro ^ (&body body)
  `(conjoin ,@body))

(defmacro v (&body body)
  `(disjoin ,@body))

(defun ~ (predicate)
  "Return function which is negation of predicate."
  (declare (optimize (speed 3) (safety 1) (debug 1)))
  (let ((predicate (ensure-function predicate)))
    (lambda (&rest arguments)
      (not (apply predicate arguments)))))
