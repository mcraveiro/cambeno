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
You can execute Lisp code by wrapping it in <lisp>...</lisp> tags.
When you do this, the result of the evaluation will be injected into your context.
Use this to verify your logic, perform complex calculations, or maintain state.
Always provide working Common Lisp code.
Once you have the final answer, provide it and then type 'STOP' to end the loop.")

(defun run-loop (initial-prompt &key (max-iterations 5))
  "Runs a persistent loop: LLM -> Middleware -> REPL -> LLM."
  (let ((current-prompt (format nil "~A~%~%User: ~A~%Assistant: " *system-instruction* initial-prompt)))
    (format t "--- Starting loop ---~%")
    (loop for i from 1 to max-iterations
          do (let* ((llm-response (query-llama current-prompt))
                    (lisp-results (eval-all-blocks llm-response)))
               (format t "~%--- Iteration ~A ---~%" i)
               (format t "LLM Response:~%~A~%~%" llm-response)
               
               (if (string= lisp-results "")
                   (progn
                     (format t "No Lisp blocks found. Loop complete.~%")
                     (return llm-response))
                   (progn
                     (format t "Lisp Results:~%~A~%~%" lisp-results)
                     ;; Update prompt: Append LLM response + result for the next iteration
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       llm-response 
                                                       (format nil "~%Results:~%~A~%Assistant: " lisp-results)))
                     (when (search "STOP" llm-response)
                       (format t "STOP signal received. Loop complete.~%")
                       (return llm-response))))))))
