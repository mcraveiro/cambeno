(in-package #:cambeno)

(defun main (&optional (input nil))
  "Simple entry point to demonstrate the processing loop."
  (if input
      (format t "~A~%" (process-llm-output input))
      (progn
        (format t "Cambeno: Persistent Lisp Metaprogramming Loop Initialized.~%")
        (format t "Example usage: (cambeno:main \"<lisp>(defun square (x) (* x x))</lisp> Result: <lisp>(square 5)</lisp>\")~%"))))
