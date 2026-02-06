(in-package #:cambeno.repl)

(defun eval-lisp-string (code-string &key (package "CAMBENO.SCRATCH"))
  "Evaluates a string of Lisp code in the specified package.
Returns a plist: (:results list :stdout string :stderr string)"
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream)))
    (handler-case
        (let* ((*standard-output* output)
               (*error-output* error-output)
               (*package* (or (find-package package) (find-package "COMMON-LISP-USER")))
               (results (multiple-value-list 
                         (eval (read-from-string (format nil "(progn ~A)" code-string))))))
          (list :results results
                :stdout (get-output-stream-string output)
                :stderr (get-output-stream-string error-output)))
      (error (e)
        (list :results nil
              :stdout (get-output-stream-string output)
              :stderr (format nil "~A" e))))))

(defun init-repl ()
  "Initialize the REPL environment."
  (unless (find-package "CAMBENO.SCRATCH")
    (make-package "CAMBENO.SCRATCH" :use '(#:cl)))
  t)
