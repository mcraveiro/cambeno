(defpackage #:cambeno.repl
  (:use #:cl)
  (:export #:eval-lisp-string
           #:init-repl))

(defpackage #:cambeno.utils
  (:use #:cl)
  (:export #:list-functions
           #:inspect-symbol))

(defpackage #:cambeno.llama
  (:use #:cl)
  (:export #:query-llama
           #:*llama-server-url*))

(defpackage #:cambeno.middleware
  (:use #:cl #:cambeno.repl #:cambeno.llama)
  (:export #:process-llm-output
           #:eval-all-blocks))

(defpackage #:cambeno
  (:use #:cl #:cambeno.repl #:cambeno.middleware #:cambeno.utils #:cambeno.llama)
  (:export #:main #:run-loop))
