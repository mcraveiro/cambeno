(in-package #:cambeno.tests)

(def-suite cambeno-suite :description "Main suite for cambeno")
(in-suite cambeno-suite)

(test test-eval-lisp-string
  (is (equal '((5) "" "") (eval-lisp-string "(+ 2 3)")))
  (is (equal '((nil) "hello
" "") (eval-lisp-string "(format t \"hello~%\")"))))

(test test-extract-lisp-blocks
  (is (equal '("(+ 1 2)" "(+ 3 4)") 
             (cambeno.middleware::extract-lisp-blocks "<lisp>(+ 1 2)</lisp> and <lisp>(+ 3 4)</lisp>"))))

(test test-process-llm-output
  (let ((result (process-llm-output "Test: <lisp>(+ 10 20)</lisp>")))
    (is (search "VALUES: (30)" result))))

(test test-query-llama
  (cl-mock:with-mocks ()
    (cl-mock:answer drakma:http-request
      (values (flexi-streams:string-to-octets "{\"content\": \"Mocked response\"}") 200 nil nil nil nil nil))
    (is (string= "Mocked response" (query-llama "Test prompt")))))

(defun run-tests ()
  (run! 'cambeno-suite))
