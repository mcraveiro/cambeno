(in-package #:cambeno.middleware)

(defun extract-lisp-blocks (text)
  "Extracts content between <lisp> and </lisp> tags."
  (let ((start-tag "<lisp>")
        (end-tag "</lisp>")
        (blocks '())
        (current-pos 0))
    (loop
      (let ((start (search start-tag text :start2 current-pos)))
        (unless start (return (nreverse blocks)))
        (let ((end (search end-tag text :start2 (+ start (length start-tag)))))
          (unless end (return (nreverse blocks)))
          (push (subseq text (+ start (length start-tag)) end) blocks)
          (setf current-pos (+ end (length end-tag))))))))

(defun process-llm-output (text)
  "Processes the LLM output, evaluating any <lisp> blocks and appending results."
  (let ((blocks (extract-lisp-blocks text))
        (processed-text text))
    (dolist (block blocks)
      (let* ((eval-result (cambeno.repl:eval-lisp-string block))
             (values (first eval-result))
             (stdout (second eval-result))
             (stderr (third eval-result))
             (result-string (format nil "~%<lisp-result>~%~@[STDOUT:~%~A~]~%~@[STDERR:~%~A~]~%VALUES: ~S~%</lisp-result>"
                                   (unless (string= stdout "") stdout)
                                   (unless (string= stderr "") stderr)
                                   values)))
        (setf processed-text (concatenate 'string processed-text result-string))))
    processed-text))
