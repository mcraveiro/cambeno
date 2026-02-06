(in-package #:cambeno.llama)

(defvar *llama-server-url* "http://localhost:8080/completion")

(defun query-llama (prompt &key (n-predict 1024) (temperature 0.7) (top-p 0.9) (repeat-penalty 1.1))
  "Sends a completion request to the llama.cpp server."
  (let* ((request-body (cl-json:encode-json-to-string
                        `(("prompt" . ,prompt)
                          ("n_predict" . ,n-predict)
                          ("temperature" . ,temperature)
                          ("top_p" . ,top-p)
                          ("repeat_penalty" . ,repeat-penalty))))
         (response-bytes (drakma:http-request *llama-server-url*
                                             :method :post
                                             :content request-body
                                             :content-type "application/json"
                                             :additional-headers '(("Accept" . "application/json")))))
    (if response-bytes
        (let* ((response-string (flexi-streams:octets-to-string response-bytes))
               (json-response (cl-json:decode-json-from-string response-string)))
          (cdr (assoc :content json-response)))
        (error "No response from llama.cpp server at ~A" *llama-server-url*))))
