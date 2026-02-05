(defpackage #:cambeno.repl
  (:use #:cl)
  (:export #:eval-lisp-string
           #:init-repl))

(defpackage #:cambeno.utils
  (:use #:cl)
  (:export #:list-functions
           #:inspect-symbol))

(defpackage #:cambeno.middleware
  (:use #:cl #:cambeno.repl)
  (:export #:process-llm-output))

(defpackage #:cambeno
  (:use #:cl #:cambeno.repl #:cambeno.middleware #:cambeno.utils)
  (:export #:main))
