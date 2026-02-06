(defpackage #:cambeno.scratch
  (:use #:cl))

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
           #:test-performance
           #:*llama-server-url*))

(defpackage #:cambeno.middleware
  (:use #:cl #:cambeno.repl #:cambeno.llama)
  (:import-from #:cl-ppcre #:scan #:regex-replace-all)
  (:import-from #:3bmd-grammar #:parse-doc)
  (:export #:markdown-to-sexp
           #:clean-llm-text
           #:log-timestamp
           #:extract-code-from-ast))

(defpackage #:cambeno
  (:use #:cl #:cambeno.repl #:cambeno.middleware #:cambeno.utils #:cambeno.llama)
  (:import-from #:cl-ppcre #:scan)
  (:export #:main #:run-loop))