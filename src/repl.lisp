(in-package #:cambeno.repl)

(defun eval-lisp-string (code-string)
  "Evaluates a string of Lisp code and returns a list containing (values-list output-string error-string)."
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (handler-case
        (let* ((*standard-output* output)
               (*error-output* error-output)
               (results (multiple-value-list 
                         (eval (read-from-string (format nil "(progn ~A)" code-string))))))
          (list results 
                (get-output-stream-string output)
                (get-output-stream-string error-output)))
      (error (e)
        (list nil 
              (get-output-stream-string output)
              (format nil "~A" e))))))

(defun init-repl ()
  "Initialize the REPL environment if needed."
  ;; For now, just a placeholder.
  t)
