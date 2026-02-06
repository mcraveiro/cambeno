(in-package #:cambeno)

(defvar *symbolic-instruction*
  "ROLE: You are a Symbolic Reasoning Engine.
ENVIRONMENT: #:CAMBENO.SCRATCH package.
COMMUNICATION: You ONLY communicate using Lisp Property Lists.
FORMAT: (:reasoning \"thought process\" :code (lisp code to execute) :stop boolean)
You may provide multiple plists in one turn if multiple steps are needed.")

(defun read-file-as-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun symbolic-run-loop (initial-task &key (max-iterations 10) (n-predict 1024))
  "Runs a persistent loop that extracts and processes all S-expressions in LLM response."
  (let ((current-prompt (format nil "~A~%~%TASK: ~A~%NEXT STEPS: " *symbolic-instruction* initial-task))
        (grammar (read-file-as-string "lisp.gbnf")))
    
    (log-timestamp "--- [START] Multi-Step Symbolic Loop ---")
    (format t "Task: ~A~%~%" initial-task)
    
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Iteration ~A] Requesting LLM Response..." i))
             (let* ((llm-response (query-llama current-prompt :grammar grammar :n-predict n-predict))
                    (sexp-strings (extract-all-sexps llm-response))
                    (turn-feedback '())
                    (stop-requested nil))
               
               (log-timestamp (format nil "--- [Iteration ~A] LLM Raw Response ---" i))
               (format t "~A~%~%" llm-response)

               (unless sexp-strings
                 (log-timestamp "ERROR: No S-Expressions found. Ending.")
                 (return (list :error "No S-Expressions")))

               (dolist (str sexp-strings)
                 (let ((data (handler-case (read-from-string str) (error (e) (list :error e)))))
                   (if (not (listp data))
                       (push (format nil "Error: ~S is not a valid list." data) turn-feedback)
                       (let ((reasoning (getf data :reasoning))
                             (code (getf data :code))
                             (stop (getf data :stop)))
                         (when reasoning (format t "Reasoning: ~A~%" reasoning))
                         (when stop (setf stop-requested t))
                         (if code
                             (let* ((wrapped-code (format nil "(in-package #:cambeno.scratch)~%~A" code))
                                    (result-json (cambeno.repl:eval-lisp-string wrapped-code))
                                    (result-data (cl-json:decode-json-from-string result-json))
                                    (values (cdr (assoc :results result-data)))
                                    (stderr (cdr (assoc :stderr result-data)))
                                    (feedback (if (and stderr (not (string= stderr "")))
                                                  (format nil "(:error ~S)" stderr)
                                                  (format nil "(:values ~S)" values))))
                               (format t "Executed: ~A~%Result: ~A~%~%" code feedback)
                               (push (format nil "Result of [~A]: ~A" code feedback) turn-feedback)))))))

               (let ((feedback-str (format nil "~%SYSTEM FEEDBACK:~%~{~A~^~%~}~%NEXT STEPS: " (nreverse turn-feedback))))
                 (setf current-prompt (concatenate 'string current-prompt llm-response feedback-str)))

               (when stop-requested
                 (log-timestamp (format nil "--- [STOP] iteration ~A ---" i))
                 (return :complete))))))

(defun run-loop (initial-task &rest args)
  "Alias for symbolic-run-loop."
  (apply #'symbolic-run-loop initial-task args))
