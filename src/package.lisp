(uiop:define-package #:cambeno.scratch
  (:use #:cl))

(uiop:define-package #:cambeno.repl
  (:use #:cl)
  (:export #:eval-lisp-string
           #:init-repl))

(uiop:define-package #:cambeno.utils
  (:use #:cl)
  (:export #:list-functions
           #:inspect-symbol))

(uiop:define-package #:cambeno.llama
  (:use #:cl)
  (:export #:query-llama
           #:test-performance
           #:*llama-server-url*
           #:*debug-llm*))

(uiop:define-package #:cambeno.middleware
  (:use #:cl #:cambeno.repl #:cambeno.llama)
  (:import-from #:cl-ppcre #:scan #:regex-replace-all)
  (:import-from #:3bmd-grammar #:parse-doc)
  (:export #:markdown-to-sexp
           #:clean-llm-text
           #:log-timestamp
           #:extract-code-from-ast
           #:find-first-sexp
           #:extract-all-sexps))

(uiop:define-package #:cambeno
  (:use #:cl #:cambeno.repl #:cambeno.middleware #:cambeno.utils #:cambeno.llama)
  (:import-from #:cl-ppcre #:scan)
  (:export #:main #:run-loop #:symbolic-run-loop))
