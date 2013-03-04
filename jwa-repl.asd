(defpackage #:jwa-repl-system
  (:use #:common-lisp #:asdf))
(in-package #:jwa-repl-system)


(asdf:defsystem #:jwa-repl
  :serial t
  :name "jwa-repl"
  :description "Collection of Common Lisp utilities for the REPL"
  :version "0.1"
  :components ((:file "repl")))
