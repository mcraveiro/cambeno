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
                 ;; 3bmd-ext-code-blocks uses (3BMD-CODE-BLOCKS::CODE-BLOCK :LANG "..." :CONTENT "...")
                 ((and (listp node) 
                       (symbolp (first node))
                       (string-equal (symbol-name (first node)) "CODE-BLOCK"))
                  (let ((lang-val (getf (cdr node) :lang)))
                    (when (and lang-val (string-equal lang-val lang))
                      (push (getf (cdr node) :content) code-blocks))))
                 ((listp node) (dolist (sub node) (traverse sub))))))
      (traverse ast)
      (nreverse code-blocks))))

(defun find-first-sexp (text)
  "Finds the first balanced (...) list in the text."
  (let ((start (search "(" text))
        (count 0)
        (pos 0))
    (when start
      (setf pos start)
      (loop
        (when (>= pos (length text)) (return nil))
        (let ((char (char text pos)))
          (cond
            ((char= char #\() (incf count))
            ((char= char #\)) (decf count)))
          (when (= count 0)
            (return (subseq text start (1+ pos))))
          (incf pos))))))

(defun markdown-to-sexp (text)
  "Converts markdown text to a Common Lisp S-expression AST using 3bmd."
  (let ((3bmd-code-blocks:*code-blocks* t))
    (3bmd-grammar:parse-doc text)))

(defun clean-llm-text (text)
  "Removes turn-based markers."
  (let ((patterns '("Assistant: " "User: ")))
    (dolist (p patterns text)
      (setf text (cl-ppcre:regex-replace-all p text "")))))
