(in-package #:cambeno.repl)

(defun eval-lisp-string (code-string &key (package "CAMBENO.SCRATCH"))
  "Compiles and evaluates a string of Lisp code.
Returns a plist: (:results list :stdout string :stderr string :warnings string)"
  (let ((output (make-string-output-stream))
        (error-output (make-string-output-stream))
        (warnings-output (make-string-output-stream)))
    (handler-case
        (let* ((*standard-output* output)
               (*error-output* error-output)
               (pkg (or (find-package package) (find-package "COMMON-LISP-USER")))
               (*package* pkg)
               (form (read-from-string (format nil "(progn ~A)" code-string)))
               ;; Compile the form to catch warnings/errors like a linter
               (compiled-fn (handler-bind 
                                ((warning (lambda (w)
                                            (format warnings-output "~A~%" w)
                                            (muffle-warning w))))
                              (compile nil `(lambda () ,form))))
               (results (multiple-value-list (funcall compiled-fn))))
          (list :results results
                :stdout (get-output-stream-string output)
                :stderr (get-output-stream-string error-output)
                :warnings (get-output-stream-string warnings-output)))
      (error (e)
        (list :results nil
              :stdout (get-output-stream-string output)
              :stderr (format nil "~A" e)
              :warnings (get-output-stream-string warnings-output))))))

(defun init-repl ()
  "Initialize the REPL environment."
  (unless (find-package "CAMBENO.SCRATCH")
    (make-package "CAMBENO.SCRATCH" :use '(#:cl)))
  t)