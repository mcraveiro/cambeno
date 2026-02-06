(in-package #:cambeno.llama)

(defvar *llama-server-url* "http://localhost:8080/completion")

(defvar *debug-llm* nil "Set to T to see raw JSON requests and responses.")

(defun query-llama (prompt &key (n-predict 1024) (temperature 0.7) (top-p 0.9) (repeat-penalty 1.1) (return-stats nil) (grammar nil))
  "Sends a completion request to the llama.cpp server. Returns (values content timings)."
  (let* ((request-body-alist `(("prompt" . ,prompt)
                               ("n_predict" . ,n-predict)
                               ("temperature" . ,temperature)
                               ("top_p" . ,top-p)
                               ("repeat_penalty" . ,repeat-penalty))))
    (when grammar
      (push `("grammar" . ,grammar) request-body-alist))
    
    (let* ((request-body (cl-json:encode-json-to-string request-body-alist)))
      (when *debug-llm*
        (format t "~%[DEBUG] Request Body: ~A~%" request-body))
      
      (let ((response-bytes (drakma:http-request *llama-server-url*
                                                :method :post
                                                :content request-body
                                                :content-type "application/json"
                                                :additional-headers '(("Accept" . "application/json")))))
        (if response-bytes
            (let* ((response-string (flexi-streams:octets-to-string response-bytes))
                   (json-response (cl-json:decode-json-from-string response-string))
                   (content (cdr (assoc :content json-response)))
                   (timings (cdr (assoc :timings json-response))))
              (when *debug-llm*
                (format t "[DEBUG] Raw Response: ~A~%" response-string))
              (if return-stats
                  (values content timings)
                  content))
            (error "No response from llama.cpp server at ~A" *llama-server-url*))))))

(defun test-performance (&optional (prompt "Write a short poem about Lisp."))
  "Runs a query and prints tokens-per-second statistics."
  (multiple-value-bind (content timings) (query-llama prompt :n-predict 128 :return-stats t)
    (let ((p-tps (cdr (assoc :prompt--per--second timings)))
          (g-tps (cdr (assoc :predicted--per--second timings)))
          (p-count (cdr (assoc :prompt--n timings)))
          (g-count (cdr (assoc :predicted--n timings))))
      (format t "~%--- Performance Stats ---~%")
      (format t "Prompt tokens: ~A (~,2F tokens/s)~%" (or p-count 0) (or p-tps 0.0))
      (format t "Gen tokens:    ~A (~,2F tokens/s)~%" (or g-count 0) (or g-tps 0.0))
      (format t "--- Response ---~%~A~%~%" content)
      (values g-tps timings))))
