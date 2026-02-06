(in-package #:cambeno.repl)

(defun eval-lisp-string (code-string)
  "Evaluates a string of Lisp code and returns a JSON string with (results stdout stderr)."
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (handler-case
        (let* ((*standard-output* output)
               (*error-output* error-output)
               (results (multiple-value-list 
                         (eval (read-from-string (format nil "(progn ~A)" code-string))))))
          (cl-json:encode-json-to-string
           `((:results . ,results)
             (:stdout . ,(get-output-stream-string output))
             (:stderr . ,(get-output-stream-string error-output)))))
      (error (e)
        (cl-json:encode-json-to-string
         `((:results . nil)
           (:stdout . ,(get-output-stream-string output))
           (:stderr . ,(format nil "~A" e))))))))

(defun init-repl ()
  "Initialize the REPL environment if needed."
  ;; For now, just a placeholder.
  t)
