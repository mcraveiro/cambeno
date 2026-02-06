(in-package #:cambeno)

(defvar *system-instruction* 
  "You are a symbolic thinking agent embedded in a persistent Common Lisp environment. 
All your code will be executed in the namespace (package) #:CAMBENO.SCRATCH.
To solve tasks, you should write code in markdown blocks (```lisp ... ```) and evaluate it to verify your results.
When you evaluate code, the result will be provided to you.
Use this environment to build complex functions turn-by-turn.
Once you have the final confirmed answer, provide it and then type 'STOP'.")

(defun main (prompt)
  "Simple demo: Query LLM and print S-exp AST."
  (log-timestamp "--- [START] Manual Query ---")
  (format t "Prompt: ~A~%" prompt)
  (let* ((raw-response (query-llama prompt :n-predict 128))
         (sexp (markdown-to-sexp raw-response)))
    (log-timestamp "--- [LLM RESPONSE] ---")
    (format t "~A~%" raw-response)
    (log-timestamp "--- [S-EXP AST] ---")
    (format t "~S~%" sexp)
    (log-timestamp "--- [END] Manual Query ---")))

(defun run-loop (initial-prompt &key (max-iterations 10) (n-predict 1024))
  "Runs a persistent loop with complete trace and timestamped logging."
  (let ((current-prompt (format nil "~A~%~%User: ~A~%Assistant: " *system-instruction* initial-prompt)))
    (log-timestamp "LOG START")
    (log-timestamp "--- [START] Autonomous Reasoning Loop ---")
    (format t "Initial User Prompt: ~A~%~%" initial-prompt)
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Iteration ~A] Requesting LLM Response..." i))
             (force-output)
             (let* ((llm-response (query-llama current-prompt :n-predict n-predict))
                    (ast (markdown-to-sexp llm-response))
                    (code-blocks (extract-code-from-ast ast))
                    (turn-results '()))
               (log-timestamp (format nil "--- [Iteration ~A] Raw LLM Markdown ---" i))
               (format t "~A~%~%" llm-response)
               (log-timestamp (format nil "--- [Iteration ~A] S-Expression AST ---" i))
               (format t "~S~%~%" ast)
               (if code-blocks
                   (progn
                     (log-timestamp (format nil "--- [Iteration ~A] Action: Executing Lisp ---" i))
                     (format t "Package Context: #:CAMBENO.SCRATCH~%~%")
                     (dolist (code code-blocks)
                       (let* ((wrapped-code (format nil "(in-package #:cambeno.scratch)~%~A" code))
                              (result-json (cambeno.repl:eval-lisp-string wrapped-code))
                              (result-data (cl-json:decode-json-from-string result-json))
                              (values (cdr (assoc :results result-data)))
                              (stdout (cdr (assoc :stdout result-data)))
                              (stderr (cdr (assoc :stderr result-data)))
                              (formatted-result (format nil "Code: ~A~%Stdout: ~A~%Stderr: ~A~%Values: ~S~%" 
                                                       code (or stdout "") (or stderr "") values)))
                         (push (format nil "Result: Stdout: ~A, Values: ~S" (or stdout "") values) turn-results)
                         (format t "Executed Code Block:~%~A~%" code)
                         (format t "Output/Result:~%~A~%" formatted-result)))
                     (log-timestamp (format nil "--- [Iteration ~A] Updating context ---" i))
                     (let ((results-string (format nil "~%Results from Lisp execution:~%~{~A~^~%~}~%Assistant: " (nreverse turn-results))))
                       (setf current-prompt (concatenate 'string 
                                                         current-prompt 
                                                         (clean-llm-text llm-response) 
                                                         results-string))))
                   (progn
                     (log-timestamp (format nil "--- [Iteration ~A] No code blocks ---" i))
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       (clean-llm-text llm-response) 
                                                       (format nil "~%Assistant: ")))))
               (cond 
                 ((cl-ppcre:scan "(?i)STOP" llm-response)
                  (log-timestamp (format nil "--- [STOP] iteration ~A ---" i))
                  (return llm-response))
                 ((and (null code-blocks) (> i 1))
                  (log-timestamp (format nil "--- [FINISH] iteration ~A ---" i))
                  (return llm-response)))))
    (log-timestamp "--- [END] Autonomous Reasoning Loop ---")
    (log-timestamp "LOG END")))
