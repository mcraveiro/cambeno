(defpackage #:cambeno.repl
  (:use #:cl #:cl-json)
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
  (:use #:cl #:cambeno.repl #:cambeno.llama #:cl-ppcre)
  (:export #:process-llm-output
           #:eval-all-blocks
           #:clean-llm-text))

(defpackage #:cambeno
  (:use #:cl #:cambeno.repl #:cambeno.middleware #:cambeno.utils #:cambeno.llama)
  (:export #:main #:run-loop))
