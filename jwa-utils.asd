
(asdf:defsystem #:jwa-utils
  :serial t
  :name "jwa-utils"
  :description "Collection of Common Lisp utilities"
  :version "1.1.0"
  :depends-on (:alexandria
               :split-sequence)
  :components ((:file "packages")
               (:file "utils")
               (:file "size")
               (:file "text")))
