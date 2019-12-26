;;;; -*- mode: Lisp; syntax: Common-Lisp; fill-column: 80; -*-
;;;; Useful common utilties, excluding those available from ALEXANDRIA.
(in-package #:jwa-utils)

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

;;; Stream processing

(defmacro dolines (function &key (stream *standard-input*) (eof-value :eof)
                              (test '#'identity))
  "Apply function to each line from stream, if :test function returns true,
collecting results into list."
  (with-gensyms (line)
    `(do ((,line (read-line ,stream nil ,eof-value)
                 (read-line ,stream nil ,eof-value)))
         ((eq ,line ,eof-value))
       (when (funcall ,test ,line)
         (funcall ,function ,line)))))


(defmacro maplines (function &key (stream *standard-input*) (eof-value :eof)
                               (test '#'identity))
  "Apply function to each line from stream, if :test function returns true,
collecting results into list."
  (with-gensyms (line list)
    `(do ((,list nil)
          (,line (read-line ,stream nil ,eof-value)
                 (read-line ,stream nil ,eof-value)))
         ((eq ,line ,eof-value) (nreverse ,list))
       (when (funcall ,test ,line)
         (push (funcall ,function ,line) ,list)))))
