(in-package #:cambeno)

(defvar *symbolic-instruction*
  "ROLE: You are a Symbolic Reasoning Engine.
ENVIRONMENT: #:CAMBENO.SCRATCH package.
COMMUNICATION: You ONLY communicate using Lisp Property Lists.
INPUT_FORMATS:
- Initial Task: (:task \"description\")
- System Feedback: (:feedback \"results or error\" :status :success|:error|:parse-error)
OUTPUT_FORMAT:
- Your response must be one or more plists: (:reasoning \"thought\" :code (lisp) :stop boolean)")

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
  "Runs a persistent loop using pure S-Expression communication."
  (let ((current-prompt (make-task-prompt initial-task))
        (grammar (read-file-as-string "lisp.gbnf")))
    (log-timestamp "================================================================================")
    (log-timestamp "--- [START] Pure Symbolic Reasoning ---")
    (format t "System Instruction:~%~A~%~%" *symbolic-instruction*)
    (format t "Initial Task: ~A~%~%" initial-task)
    (loop for i from 1 to max-iterations
          do (log-timestamp (format nil ">>> [Step ~A] Requesting LLM Response..." i))
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
                                                       (make-feedback-prompt "No balanced S-Expressions found." :parse-error))))
                   (progn
                     (dolist (str sexp-strings)
                       (multiple-value-bind (data err)
                           (handler-case (values (read-from-string str) nil)
                             (error (e) (values nil e)))
                         (if err
                             (progn
                               (log-timestamp (format nil "--- [Step ~A] Parse Error in Sexp ---" i))
                               (setf has-error t)
                               (push (format nil "Parse error in ~S: ~A" str err) turn-feedback))
                             (let ((reasoning (getf data :reasoning))
                                   (code (getf data :code))
                                   (stop (getf data :stop)))
                               (when reasoning (format t "Reasoning: ~A~%" reasoning))
                               (when stop (setf stop-requested t))
                               (if code
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
                                     (push (format nil "Result of ~S: ~A" code feedback-val) turn-feedback)))))))
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
                  (log-timestamp (format nil "--- [ABORT] Repeated parse failures at step ~A ---" i))
                  (return :failed))))))
  (log-timestamp "--- [END] Pure Symbolic Reasoning ---")
  (log-timestamp "================================================================================"))

(defun run-loop (initial-task &rest args)
  "Alias for symbolic-run-loop."
  (apply #'symbolic-run-loop initial-task args))