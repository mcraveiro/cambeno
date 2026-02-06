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
Always aim to provide working Common Lisp code.")

(defun run-loop (initial-prompt &key (max-iterations 5))
  "Runs a persistent loop: LLM -> Middleware -> REPL -> LLM."
  (let ((current-prompt (format nil "~A~%~%User: ~A" *system-instruction* initial-prompt)))
    (format t "Starting loop...~%")
    (loop for i from 1 to max-iterations
          do (format t "--- Iteration ~A ---~%" i)
             (let* ((llm-response (query-llama current-prompt))
                    (processed-response (process-llm-output llm-response)))
               (format t "LLM Response:~%~A~%~%" llm-response)
               (if (string= llm-response processed-response)
                   (progn
                     (format t "No more Lisp blocks to process. Ending loop.~%")
                     (return processed-response))
                   (progn
                     (format t "Processed Response (with results):~%~A~%~%" processed-response)
                     ;; Update prompt for the next turn, including previous state
                     (setf current-prompt (concatenate 'string current-prompt llm-response processed-response))))))))
