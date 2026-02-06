(defsystem "cambeno"
  :version "0.1.0"
  :author "Marco"
  :license "MIT"
  :depends-on ("uiop" "cl-json" "drakma" "flexi-streams" "str" "cl-ppcre")
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "repl")
                 (:file "utils")
                 (:file "llama")
                 (:file "middleware")
                 (:file "main"))))
  :description "Implementation of 'From Tool Calling to Symbolic Thinking' architecture."
  :in-order-to ((test-op (test-op "cambeno/tests"))))

(defsystem "cambeno/tests"
  :author "Marco"
  :license "MIT"
  :depends-on ("cambeno" "fiveam" "cl-mock")
  :components ((:module "tests"
                :components
                ((:file "package")
                 (:file "main"))))
  :description "Test system for cambeno"
  :perform (test-op (op c) (symbol-call :cambeno.tests :run-tests)))
