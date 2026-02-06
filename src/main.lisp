(in-package #:cambeno)

(defvar *system-instruction* 
  "ROLE: You are a Symbolic Reasoning Engine. You solve tasks by writing and verifying Common Lisp code.
ENVIRONMENT: You are currently inside the #:CAMBENO.SCRATCH package.
RULES:
1. ONLY output Markdown.
2. For code verification, use markdown blocks: ```lisp ... ```.
3. DO NOT simulate turn-taking. NEVER write 'User:', 'Assistant:', or 'CL-USER>'.
4. DO NOT make up results. Execution results will be provided to you by the system in the next turn.
5. If an error is reported, fix the code logic.
6. Once the task is fully verified and complete, provide the final answer and end with the word 'STOP'.")

(defun main (prompt)
  "Simple demo: Query LLM and print S-exp AST."
  (log-timestamp "--- [START] Manual Query ---")
  (format t "Prompt: ~A~%" prompt)
  (let* ((raw-response (query-llama (format nil "~A~%~%Task: ~A" *system-instruction* prompt) :n-predict 512))
         (sexp (markdown-to-sexp raw-response)))
    (log-timestamp "--- [LLM RESPONSE] ---")
    (format t "~A~%" raw-response)
    (log-timestamp "--- [S-EXP AST] ---")
    (format t "~S~%" sexp)
    (log-timestamp "--- [END] Manual Query ---")))

(defun run-loop (initial-prompt &key (max-iterations 10) (n-predict 1024))
  "Runs a persistent loop with complete trace and timestamped logging."
  (let ((current-prompt (format nil "~A~%~%TASK: ~A~%REASONING: " *system-instruction* initial-prompt)))
    
    (log-timestamp "================================================================================")
    (log-timestamp "--- [START] Autonomous Reasoning Loop ---")
    (format t "Task: ~A~%~%" initial-prompt)
    
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Iteration ~A] Requesting LLM Response..." i))
             (force-output)
             
             (let* ((llm-response (query-llama current-prompt :n-predict n-predict))
                    (ast (markdown-to-sexp llm-response))
                    (code-blocks (extract-code-from-ast ast))
                    (turn-results '()))
               
               (log-timestamp (format nil "--- [Iteration ~A] Raw LLM Markdown ---" i))
               (format t "~A~%~%" llm-response)
               
               (if code-blocks
                   (progn
                     (log-timestamp (format nil "--- [Iteration ~A] Action: Executing Lisp ---" i))
                     (dolist (code code-blocks)
                       ;; Filter out common hallucinated REPL prefixes
                       (let* ((clean-code (cl-ppcre:regex-replace-all "(?m)^CL-USER>\\s*" code ""))
                              (wrapped-code (format nil "(in-package #:cambeno.scratch)~%~A" clean-code))
                              (result-json (cambeno.repl:eval-lisp-string wrapped-code))
                              (result-data (cl-json:decode-json-from-string result-json))
                              (values (cdr (assoc :results result-data)))
                              (stdout (cdr (assoc :stdout result-data)))
                              (stderr (cdr (assoc :stderr result-data)))
                              (formatted-output (if (and stderr (not (string= stderr "")))
                                                    (format nil "ERROR: ~A" stderr)
                                                    (format nil "Stdout: ~A~%Values: ~S" (or stdout "") values))))
                         (push (format nil "Output of [~A]:~%~A" clean-code formatted-output) turn-results)
                         (format t "Code:~%~A~%" clean-code)
                         (format t "Result:~%~A~%~%" formatted-output)))
                     
                     (log-timestamp (format nil "--- [Iteration ~A] Updating context ---" i))
                     (let ((results-string (format nil "~%SYSTEM FEEDBACK:~%~{~A~^~%~}~%REASONING: " (nreverse turn-results))))
                       (setf current-prompt (concatenate 'string 
                                                         current-prompt 
                                                         llm-response 
                                                         results-string))))
                   
                   (progn
                     (log-timestamp (format nil "--- [Iteration ~A] No code blocks ---" i))
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       llm-response 
                                                       (format nil "~%REASONING: ")))))

               (cond 
                 ((cl-ppcre:scan "(?i)STOP" llm-response)
                  (log-timestamp (format nil "--- [STOP] iteration ~A ---" i))
                  (return llm-response))
                 
                 ((and (null code-blocks) (> i 1))
                  (log-timestamp (format nil "--- [FINISH] iteration ~A ---" i))
                  (return llm-response))))))
    
    (log-timestamp "--- [END] Autonomous Reasoning Loop ---")
    (log-timestamp "================================================================================")))
