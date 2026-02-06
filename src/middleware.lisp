(in-package #:cambeno.middleware)

(defun log-timestamp (msg &optional (stream *standard-output*))
  "Prints a timestamped message."
  (multiple-value-bind (sec min hour day month year) (get-decoded-time)
    (format stream "[~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D] ~A~%"
            year month day hour min sec msg)))

(defun extract-code-from-ast (ast &key (lang "lisp"))
  "Recursively extracts all code block content for a specific language from 3bmd AST."
  (let ((code-blocks '()))
    (labels ((traverse (node)
               (cond 
                 ((and (listp node) (eq (first node) :code))
                  (let ((meta (second node)))
                    (when (string-equal (getf meta :lang) lang)
                      (push (getf meta :content) code-blocks))))
                 ((listp node) (dolist (sub node) (traverse sub))))))
      (traverse ast)
      (nreverse code-blocks))))

(defun markdown-to-sexp (text)
  "Converts markdown text to a Common Lisp S-expression AST using 3bmd."
  (let ((3bmd-code-blocks:*code-blocks* t))
    (3bmd-grammar:parse-doc text)))

(defun clean-llm-text (text)
  "Removes turn-based markers."
  (let ((patterns '("Assistant: " "User: ")))
    (dolist (p patterns text)
      (setf text (cl-ppcre:regex-replace-all p text "")))))