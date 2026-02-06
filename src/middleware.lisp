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

(defun eval-all-blocks (text)
  "Evaluates all Lisp blocks in text and returns a single string containing all results."
  (let ((blocks (extract-lisp-blocks text))
        (results-list '()))
    (dolist (block blocks)
      (let* ((json-result (cambeno.repl:eval-lisp-string block))
             (data (cl-json:decode-json-from-string json-result))
             (results (cdr (assoc :results data)))
             (stdout (cdr (assoc :stdout data)))
             (stderr (cdr (assoc :stderr data)))
             (result-string (format nil "<lisp-result>~%~@[STDOUT:~%~A~]~%~@[STDERR:~%~A~]~%VALUES: ~S~%</lisp-result>~%"
                                   (unless (string= stdout "") stdout)
                                   (unless (string= stderr "") stderr)
                                   results)))
        (push result-string results-list)))
    (format nil "~{~A~^~%~}" (nreverse results-list))))

(defun clean-llm-text (text)
  "Removes artifacts from LLM text that shouldn't persist in context."
  (let ((patterns '("The result will be provided to you in the next turn"
                    "STOP"
                    "Assistant: ")))
    (dolist (p patterns text)
      (setf text (cl-ppcre:regex-replace-all p text "")))))

(defun md-to-lisp-tags (text)
  "Converts markdown code blocks (```lisp ... ```) to <lisp>...</lisp> tags."
  (let ((regex "```lisp\\s*(.*?)\\s*```")
        (processed text))
    (loop
      (multiple-value-bind (match-start match-end reg-starts reg-ends)
          (cl-ppcre:scan regex processed)
        (unless match-start (return processed))
        (let ((code (subseq processed (aref reg-starts 0) (aref reg-ends 0))))
          (setf processed (concatenate 'string
                                       (subseq processed 0 match-start)
                                       "<lisp>"
                                       code
                                       "</lisp>"
                                       (subseq processed match-end))))))))

(defun process-llm-output (text)
  "Processes the LLM output, evaluating any <lisp> blocks and appending results."
  (let* ((pre-processed (md-to-lisp-tags text))
         (results (eval-all-blocks pre-processed)))
    (if (string= results "")
        text
        (concatenate 'string text (format nil "~%~A" results)))))
