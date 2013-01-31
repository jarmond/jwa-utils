(in-package #:cl-user)
(defpackage #:jwa-utils
  (:use #:common-lisp)
  (:export
   
   last1
   singleton-p
   append1
   conc1
   mklist
   
   longer
   filter
   group
   
   prune
   
   find-with-values
   before
   after
   duplicate
   split-if
   
   most
   best
   mostn
   
   mapa-b
   map1-n
   map0-n
   map->
   mapcars
   rmapcar

   readlist
   prompt
   break-loop

   mkstr
   reread
   explode

   while
   until
   for
   maplines
   string-cat))

(defpackage #:jwa-utils.size
  (:use #:cl
        #:jwa-utils)
  (:export
   bytes-prefixed-string))
