(in-package #:cambeno)

(defvar *system-instruction* 
  "You are a symbolic thinking agent embedded in a persistent Common Lisp environment. 
Your output should be valid Markdown.
Once you have the final confirmed answer, provide it and then type 'STOP'.")

(defun main (prompt)
  "Simple demo: Query LLM, clean output, convert to S-exp, and print."
  (let* ((raw-response (query-llama prompt :n-predict 128))
         (cleaned (clean-llm-text raw-response))
         (sexp (markdown-to-sexp cleaned)))
    (format t "Raw Response:~%~A~%~%" raw-response)
    (format t "S-Expression AST:~%~S~%~%" sexp)))

(defun run-loop (initial-prompt &key (max-iterations 10) (n-predict 512))
  "Runs a simple loop that queries the LLM and parses it to S-exp AST."
  (let ((current-prompt (format nil "~A~%~%User: ~A~%Assistant: " *system-instruction* initial-prompt)))
    (format t "--- Starting LLM Loop ---~%")
    (loop for i from 1 to max-iterations
          do (format t "~%[Iteration ~A] Requesting LLM response...~%" i)
             (force-output)
             (let* ((llm-response (query-llama current-prompt :n-predict n-predict))
                    (sexp (markdown-to-sexp llm-response))
                    (cleaned-response (clean-llm-text llm-response)))
               
               (format t "[Iteration ~A] LLM Response:~%~A~%~%" i llm-response)
               (format t "[Iteration ~A] S-Exp AST:~%~S~%~%" i sexp)
               
               (cond 
                 ((cl-ppcre:scan "(?i)STOP" llm-response)
                  (format t "[Iteration ~A] STOP signal received. Loop complete.~%" i)
                  (return llm-response))

                 (t
                  ;; Update prompt for next turn
                  (setf current-prompt (concatenate 'string 
                                                    current-prompt 
                                                    cleaned-response 
                                                    (format nil "~%Assistant: ")))))))))
