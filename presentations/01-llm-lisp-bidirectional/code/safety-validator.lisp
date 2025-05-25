(defpackage :bidirectional-llm/safety
  (:use :cl)
  (:export :validate-generated-code :safe-eval))

(in-package :bidirectional-llm/safety)

(defvar *dangerous-symbols* 
  '(delete-file delete-directory format-hard-disk
    eval compile load require
    sb-ext:run-program uiop:run-program))

(defvar *allowed-packages*
  '(:cl :alexandria :bidirectional-llm))

(defun validate-generated-code (code-string)
  "Validate that generated code is safe to evaluate"
  (handler-case
      (let ((parsed (read-from-string code-string)))
        (and (validate-symbols parsed)
             (validate-packages parsed)
             (validate-structure parsed)))
    (error (c)
      (warn "Code validation failed: ~A" c)
      nil)))

(defun validate-symbols (form)
  "Check for dangerous symbols in the form"
  (typecase form
    (symbol (not (member form *dangerous-symbols*)))
    (cons (and (validate-symbols (car form))
               (validate-symbols (cdr form))))
    (t t)))

(defun safe-eval (code-string context)
  "Safely evaluate generated code with limited privileges"
  (when (validate-generated-code code-string)
    (let ((*package* (find-package :bidirectional-llm))
          (*read-eval* nil))
      (handler-case
          (eval (read-from-string code-string))
        (error (c)
          (warn "Evaluation error: ~A" c)
          nil)))))
