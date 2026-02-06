(in-package #:cambeno)

(defvar *symbolic-instruction*
  "ROLE: You are a Symbolic Reasoning Engine.
ENVIRONMENT: #:CAMBENO.SCRATCH package.
COMMUNICATION: You ONLY communicate using Lisp Property Lists.
FORMAT: (:reasoning \"thought process\" :code (lisp code to execute) :stop boolean)
EXAMPLE: (:reasoning \"Checking if 4 is prime\" :code (is-prime 4) :stop nil)")

(defun read-file-as-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun symbolic-run-loop (initial-task &key (max-iterations 10))
  "Runs a pure symbolic loop using GBNF grammar to enforce S-exp responses."
  (let ((current-prompt (format nil "~A~%~%TASK: ~A~%NEXT STEP: " *symbolic-instruction* initial-task))
        (grammar (read-file-as-string "lisp.gbnf")))
    
    (log-timestamp "--- [START] Pure Symbolic Loop ---")
    (format t "Task: ~A~%~%" initial-task)
    
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Iteration ~A] Requesting S-Exp from LLM..." i))
             (let* ((llm-response (query-llama current-prompt :grammar grammar))
                    (data (handler-case (read-from-string llm-response) (error (e) (list :error e)))))
               
               (log-timestamp (format nil "--- [Iteration ~A] LLM S-Expression ---" i))
               (format t "~S~%~%" data)
               
               (let ((reasoning (getf data :reasoning))
                     (code (getf data :code))
                     (stop (getf data :stop)))
                 
                 (when reasoning (format t "Reasoning: ~A~%" reasoning))
                 
                 (if code
                     (progn
                       (log-timestamp (format nil "--- [Iteration ~A] Action: Executing Code ---" i))
                       (let* ((wrapped-code (format nil "(in-package #:cambeno.scratch)~%~A" code))
                              (result-json (cambeno.repl:eval-lisp-string wrapped-code))
                              (result-data (cl-json:decode-json-from-string result-json))
                              (values (cdr (assoc :results result-data)))
                              (stderr (cdr (assoc :stderr result-data)))
                              (feedback (if (and stderr (not (string= stderr "")))
                                            (format nil "(:error ~S)" stderr)
                                            (format nil "(:values ~S)" values))))
                         (format t "Result: ~A~%~%" feedback)
                         (setf current-prompt (concatenate 'string 
                                                           current-prompt 
                                                           llm-response 
                                                           (format nil "~%SYSTEM FEEDBACK: ~A~%NEXT STEP: " feedback)))))
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       llm-response 
                                                       (format nil "~%NEXT STEP: "))))

                 (when (or stop (getf data :error))
                   (log-timestamp (format nil "--- [FINISH] Loop complete at iteration ~A ---" i))
                   (return data))))))
    (log-timestamp "--- [END] Pure Symbolic Loop ---")))
