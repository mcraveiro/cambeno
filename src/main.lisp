(in-package #:cambeno)

(defvar *symbolic-instruction*
  "ROLE: You are a Symbolic Reasoning Engine.
ENVIRONMENT: #:CAMBENO.SCRATCH package.
COMMUNICATION: You ONLY communicate using Lisp Property Lists.
FORMAT: 
(:approach \"overall strategy\"
 :steps (\"step 1\" \"step 2\" ...)
 :code (lisp code to execute or nil)
 :stop boolean)

RULES:
1. Always provide the :approach and :steps.
2. The :code field should contain executable Common Lisp.
3. Once the task is complete and results are verified, set :stop to t.
4. If you receive an error or warning, refine your :approach and fix the :code.")

(defun read-file-as-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun make-task-prompt (task)
  (format nil "~A~%~%(:task ~S)~%REASONING: " *symbolic-instruction* task))

(defun make-feedback-prompt (feedback status)
  (format nil "~%(:feedback ~S :status ~S)~%REASONING: " feedback status))

(defun symbolic-run-loop (initial-task &key (max-iterations 10) (n-predict 1024))
  "Runs a persistent loop using structured symbolic communication."
  (let ((current-prompt (make-task-prompt initial-task))
        (grammar (read-file-as-string "lisp.gbnf")))
    
    (log-timestamp "================================================================================")
    (log-timestamp "--- [START] Pure Symbolic Reasoning ---")
    (format t "System Instruction:~%~A~%~%" *symbolic-instruction*)
    (format t "Initial Task: ~A~%~%" initial-task)
    
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Step ~A] Requesting S-Exp from LLM..." i))
             (force-output)
             
             (let* ((llm-response (query-llama current-prompt :grammar grammar :n-predict n-predict))
                    (sexp-strings (extract-all-sexps llm-response))
                    (turn-feedback '())
                    (stop-requested nil)
                    (has-error nil))
               
               (log-timestamp (format nil "--- [Step ~A] LLM Raw Response ---" i))
               (format t "~A~%~%" llm-response)

               (if (null sexp-strings)
                   (progn
                     (log-timestamp (format nil "--- [Step ~A] Error: No valid S-Expressions ---" i))
                     (setf current-prompt (concatenate 'string 
                                                       current-prompt 
                                                       llm-response 
                                                       (make-feedback-prompt "No balanced S-Expressions found. Use the (:approach ... :steps ...) format." :parse-error))))
                   
                   (progn
                     (dolist (str sexp-strings)
                       (multiple-value-bind (data err)
                           (handler-case (values (read-from-string str) nil)
                             (error (e) (values nil e)))
                         (if (or err (not (listp data)) (not (keywordp (first data))))
                             (progn
                               (log-timestamp (format nil "--- [Step ~A] Format/Parse Error ---" i))
                               (setf has-error t)
                               (push (format nil "Format error in ~S. Ensure it is a PLIST starting with a keyword." str) turn-feedback))
                             (let ((approach (getf data :approach))
                                   (steps (getf data :steps))
                                   (code (getf data :code))
                                   (stop (getf data :stop)))
                               
                               (when approach (format t "Approach: ~A~%" approach))
                               (when steps (format t "Steps: ~{~%  - ~A~}~%~%" steps))
                               
                               (when stop (setf stop-requested t))
                               
                               (if (and code (not (eq code 'nil)))
                                   (let* ((exec-result (cambeno.repl:eval-lisp-string (format nil "~S" code)))
                                          (values (getf exec-result :results))
                                          (stdout (getf exec-result :stdout))
                                          (stderr (getf exec-result :stderr))
                                          (warnings (getf exec-result :warnings))
                                          (feedback-val (cond 
                                                          ((and stderr (not (string= stderr "")))
                                                           (progn (setf has-error t) (format nil "ERROR: ~A" stderr)))
                                                          ((and warnings (not (string= warnings "")))
                                                           (format nil "WARNINGS:~%~A~%VALUES: ~S~@[~%STDOUT: ~A~]" warnings values stdout))
                                                          (t 
                                                           (format nil "VALUES: ~S~@[~%STDOUT: ~A~]" values stdout)))))
                                     (format t "Code: ~S~%Result: ~A~%~%" code feedback-val)
                                     (push (format nil "Result of ~S: ~A" code feedback-val) turn-feedback))
                                   (when (not stop)
                                     (push "Plan acknowledged. Please proceed to execute the code steps." turn-feedback))))))

                     (let* ((status (if has-error :error :success))
                            (feedback-str (make-feedback-prompt 
                                           (format nil "~{~A~^~%~}" (nreverse turn-feedback))
                                           status)))
                       (setf current-prompt (concatenate 'string current-prompt llm-response feedback-str)))))

               (cond 
                 (stop-requested
                  (log-timestamp (format nil "--- [STOP] Completion signal received at step ~A ---" i))
                  (return :complete))
                 
                 ((and (null sexp-strings) (> i 3))
                  (log-timestamp (format nil "--- [ABORT] Repeated failures at step ~A ---" i))
                  (return :failed))))))
    
    (log-timestamp "--- [END] Pure Symbolic Reasoning ---")
    (log-timestamp "================================================================================")))
