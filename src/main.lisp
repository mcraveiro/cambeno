(in-package #:cambeno)

(defun main (&optional (input nil))
  "Simple entry point to demonstrate the processing loop."
  (if input
      (format t "~A~%" (process-llm-output input))
      (progn
        (format t "Cambeno: Persistent Lisp Metaprogramming Loop Initialized.~%")
        (format t "Example usage: (cambeno:main \"<lisp>(defun square (x) (* x x))</lisp> Result: <lisp>(square 5)</lisp>\")~%"))))

(defvar *system-instruction* 
  "You are a symbolic thinking agent embedded in a persistent Common Lisp environment. 
To execute code, you MUST wrap it in <lisp>...</lisp> tags. 
DO NOT use markdown code blocks (like ```lisp) for code you want to execute.
Example: To add 2 and 2, write <lisp>(+ 2 2)</lisp>.
The result will be provided to you in the next turn.
Once you have the final answer, provide it and then type 'STOP'.")

(defun run-loop (initial-prompt &key (max-iterations 5))
  "Runs a persistent loop: LLM -> Middleware -> REPL -> LLM."
  (let ((current-prompt (format nil "~A~%~%User: ~A~%Assistant: " *system-instruction* initial-prompt)))
    (format t "--- Starting loop ---~%")
    (loop for i from 1 to max-iterations
          do (let* ((llm-response (query-llama current-prompt))
                    (lisp-results (eval-all-blocks llm-response))
                    (cleaned-response (clean-llm-text llm-response)))
               (format t "~%--- Iteration ~A ---~%" i)
               (format t "LLM Response:~%~A~%~%" llm-response)
               
               (if (string= lisp-results "")
                   (progn
                     (format t "No Lisp blocks found. Loop complete.~%")
                     (return llm-response))
                   (progn
                     (format t "Lisp Results:~%~A~%~%" lisp-results)
                     ;; Update prompt: Append cleaned response + result
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       cleaned-response 
                                                       (format nil "~%Results:~%~A~%Assistant: " lisp-results)))
                     (when (search "STOP" llm-response)
                       (format t "STOP signal received. Loop complete.~%")
                       (return llm-response))))))))
