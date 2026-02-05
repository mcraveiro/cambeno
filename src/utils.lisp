(in-package #:cambeno.utils)

(defun list-functions (&optional (package *package*))
  "Lists all exported functions in a package."
  (let ((pkg (if (packagep package) package (find-package package)))
        (result '()))
    (when pkg
      (do-external-symbols (sym pkg)
        (when (fboundp sym)
          (push sym result))))
    (sort result #'string< :key #'symbol-name)))

(defun inspect-symbol (symbol-name)
  "Describes a symbol."
  (let ((sym (if (symbolp symbol-name) 
                 symbol-name 
                 (read-from-string (string symbol-name)))))
    (with-output-to-string (*standard-output*)
      (describe sym))))
