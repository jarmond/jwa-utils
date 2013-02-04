;;;; -*- mode: Lisp; syntax: Common-Lisp; fill-column: 80; -*-
;;;; Useful common utilties, excluding those available from ALEXANDRIA.
(in-package #:jwa-utils)

;;; Utilities from On Lisp, Paul Graham (with some changes).

;;; Fig. 4.1

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  (car (last lst)))

(defun singleton-p (lst)
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  (append lst (list obj)))

(defun conc1 (lst obj)
  (nconc lst (list obj)))

(defun mklist (obj)
  (if (listp obj) obj (list obj)))

;;; Fig 4.2

(defun longer (x y)
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons (subseq source 0 n) acc))
                   (nreverse (cons source acc))))))
    (if source (rec source nil) nil)))

;;; Fig 4.3

;; In ALEXANDRIA
;; (defun flatten (x)
;;   (labels ((rec (x acc)
;;              (cond ((null x) acc)
;;                    ((atom x) (cons x acc))
;;                    (t (rec (car x) (rec (cdr x) acc))))))
;;     (rec x nil)))

(defun prune (test tree)
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))

;;; Fig 4.4

(defun find-with-values (fn lst)
  "Find first element for which fn returns non-nil, and return result."
  (if (null lst)
      nil
      (let ((val (funcall fn (car lst))))
        (if val
            (values (car lst) val)
            (find-with-values fn (cdr lst))))))

(defun before (x y lst &key (test #'eql))
  "Return nil if y occurs in lst before x, otherwise returns cdr
   beginning with x."
  (and lst
       (let ((first (car lst)))
         (cond ((funcall test y first) nil)
               ((funcall test x first) lst)
               (t (before x y (cdr lst) :test test))))))

(defun after (x y lst &key (test #'eql))
  "Return nil if x does not occur after y in lst, otherwise returns
   cdr beginning with x."
  (let ((rest (before y x lst :test test)))
    (and rest (member x rest :test test))))

(defun duplicate (obj lst &key (test #'eql))
  "Tests for duplication of obj in lst, and returns cdr beginning with
   second obj."
  (member obj (cdr (member obj lst :test test))
          :test test))

(defun split-if (fn lst)
  "Returns two halves of lst split at first element satisfying fn."
  (let ((acc nil))
    (do ((src lst (cdr src)))
        ((or (null src) (funcall fn (car src)))
         (values (nreverse acc) src))
      (push (car src) acc))))

;;; Fig 4.5

(defun most (fn lst)
  "Return element scoring numerically highest under fn. Also returns
   winning score."
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score max)
              (setq wins obj
                    max score))))
        (values wins max))))

(defun best (fn lst)
  "Returns element which is higher than all others, under predicate fn."
  (if (null lst)
      nil
      (let ((wins (car lst)))
        (dolist (obj (cdr lst))
          (if (funcall fn obj wins)
              (setq wins obj)))
        wins)))

(defun mostn (fn lst)
  "Returns list of all elements with highest score, under fn."
  (if (null lst)
      (values nil nil)
      (let ((result (list (car lst)))
            (max (funcall fn (car lst))))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (cond ((> score max) (setq max score
                                      result (list obj)))
                  ((= score max) (push obj result)))))
        (values (nreverse result) max))))

;;; Fig 4.6

(defun mapa-b (fn a b &optional (step 1))
  "Apply fn to integers in range a to b and return list of results."
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun map-> (fn start test-fn succ-fn)
  "Apply fn to start, followed by objects returned by succ-fn, until
   test-fn returns true, and return result"
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

;; mappend supplied by ALEXANDRIA

(defun mapcars (fn &rest lsts)
  "MAPCAR for concatenation of multiple lists."
  (let ((result nil))
    (dolist (lst lsts)
      (dolist (obj lst)
        (push (funcall fn obj) result)))
    (nreverse result)))

(defun rmapcar (fn &rest args)
  "MAPCAR for trees."
  (if (some #'atom args)
      (apply fn args)
      (apply #'mapcar
             #'(lambda (&rest args)
                 (apply #'rmapcar fn args))
             args)))

;;; Fig 4.7

(defun readlist (&rest args)
  "Breaks line of input into list."
  (values (read-from-string
           (concatenate 'string "(" (apply #'read-line args) ")"))))

(defun prompt (&rest args)
  "Prints question and reads answer back. Args as for FORMAT."
  (apply #'format *query-io* args)
  (read *query-io*))

(defun break-loop (fn quit &rest args)
  "Call fn until response satisfies quit predicate."
  (format *query-io* "Entering break-loop.~%")
  (loop
     (let ((in (apply #'prompt args)))
       (if (funcall quit in)
           (return)
           (format *query-io* "~A~%" (funcall fn in))))))

;;; Fig 4.8

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; symb supplied by ALEXANDRIA as symbolicate

(defun reread (&rest args)
  "Converts args to concatenated string and reads back as Lisp object."
  (values (read-from-string (apply #'mkstr args))))

(defun explode (sym)
  "Breaks a symbol's name into a list of symbols made from the characters."
  (map 'list #'(lambda (c) (intern (make-string 1 :initial-element c)))
       (symbol-name sym)))
                 


;;; End of On Lisp utilities


(defmacro with-gensyms (vars &body body)
  `(let ,(loop for var in vars collecting `(,var (gensym)))
     ,@body))

;;; Control structures

(defmacro while (test &body body)
  `(do () ((not ,test)) ,@body))

(defmacro until (test &body body)
  `(do () (,test) ,@body))

(defmacro for ((var start end) &body body)
  (with-gensyms (limit)
      `(do ((,var ,start (1+ ,var))
            (,limit ,end))
           ((> ,var ,limit))
         ,@body)))


;;; Mapping functions

(defmacro maplines (function &optional (stream *standard-input*) (eof-value :eof))
  (with-gensyms (line)
    `(do ((,line (read-line ,stream nil ,eof-value) (read-line ,stream nil ,eof-value)))
         ((eq ,line ,eof-value))
       (funcall ,function ,line))))


;;; String functions

(defun string-cat (&rest strings)
  "Concatenates sequence of strings."
  (apply #'concatenate 'string strings))

(defun string-join (joiner string-list)
  "Concatenates list of strings, with joiner as a delimiter."
  (string-cat
   (apply #'string-cat (mapcar (lambda (s) (concatenate 'string s joiner))
                               (subseq string-list 0 (1- (length string-list)))))
   (last1 string-list)))

(defun count-occurences (pattern string)
  (loop
     with start = 0
     with count = 0
     while start do
       (when (setf start (search pattern string :start2 start))
         (incf count)
         (incf start (length pattern)))
     finally (return count)))

;;; Plists

(defun getf-many (plist keys)
  "Get list of values associated with keys."
  (loop while plist 
     for (key value tail) = (get-properties plist keys)
     collecting value
     do (setf plist (cddr tail))))

(defun plist-values (plist)
  "Get all values from a plist."
  (loop while plist
     for value = (cadr plist)
     collecting value
     do (setf plist (cddr plist))))

;;; Definition macros

; defvar replacement with make- and reset- functions.


;;; Counter

(defun make-counter ()
  (let ((counter 0))
    (lambda (&optional reset)
      (if reset
          (setf counter 0)
          (incf counter)))))

;;; User interaction

(defun yes-no-ignore-or-quit (&optional control &rest arguments)
  "Ask question and parse response from user, allowing yes, no, ignore or quit
responses. Returns :yes, :no, :ignore, or :quit symbols."
  (loop
     with response
     do
       (when control
         (fresh-line)
         (format *query-io* control arguments))
       (write-string " (Y, N, I or Q) " *query-io*)
       (setf response (string-downcase (read-line)))
       (when (> (length response) 0)
         (case (elt response 0)
           (#\y (return :yes))
           (#\n (return :no))
           (#\i (return :ignore))
           (#\q (return :quit))
           (otherwise (progn
                        (fresh-line)
                        (write-string "Please type \"y\" for yes, \"n\" for no, \"i\" ignore or \"q\" for quit.")))))))
  
