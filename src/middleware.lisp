(in-package #:cambeno.middleware)

(defun markdown-to-sexp (text)
  "Converts markdown text to a Common Lisp S-expression AST using 3bmd."
  (let ((3bmd-code-blocks:*code-blocks* t))
    (3bmd-grammar:parse-doc text)))

(defun clean-llm-text (text)
  "Removes turn-based markers."
  (let ((patterns '("Assistant: " "User: ")))
    (dolist (p patterns text)
      (setf text (cl-ppcre:regex-replace-all p text "")))))