(in-package #:cambeno.middleware)

(defun extract-all-sexps (text)
  "Extracts all balanced top-level (...) lists from text, ignoring contents of strings."
  (let ((sexps '())
        (pos 0)
        (len (length text)))
    (loop
      (let ((start (search "(" text :start2 pos)))
        (unless start (return (nreverse sexps)))
        (let ((count 0)
              (in-string nil)
              (escaped nil)
              (end nil))
          (loop for i from start below len do
            (let ((char (char text i)))
              (cond
                (escaped (setf escaped nil))
                ((char= char #\\) (setf escaped t))
                ((char= char #\") (setf in-string (not in-string)))
                ((not in-string)
                 (cond
                   ((char= char #\() (incf count))
                   ((char= char #\)) (decf count)))))
              (when (and (not in-string) (= count 0))
                (setf end i)
                (return))))
          (if end
              (progn
                (push (subseq text start (1+ end)) sexps)
                (setf pos (1+ end)))
              (return (nreverse sexps))))))))

(defun find-first-sexp (text)
  "Legacy support for finding just one sexp."
  (first (extract-all-sexps text)))

(defun markdown-to-sexp (text)
  "Converts markdown text to a Common Lisp S-expression AST using 3bmd."
  (let ((3bmd-code-blocks:*code-blocks* t))
    (3bmd-grammar:parse-doc text)))

(defun clean-llm-text (text)
  "Removes turn-based markers."
  (let ((patterns '("Assistant: " "User: ")))
    (dolist (p patterns text)
      (setf text (cl-ppcre:regex-replace-all p text "")))))

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
                 ((and (listp node) 
                       (symbolp (first node))
                       (string-equal (symbol-name (first node)) "CODE-BLOCK"))
                  (let ((lang-val (getf (cdr node) :lang)))
                    (when (and lang-val (string-equal lang-val lang))
                      (push (getf (cdr node) :content) code-blocks))))
                 ((listp node) (dolist (sub node) (traverse sub))))))
      (traverse ast)
      (nreverse code-blocks))))