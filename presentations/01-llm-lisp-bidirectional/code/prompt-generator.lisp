(defpackage :bidirectional-llm/prompts
  (:use :cl)
  (:export :generate-prompt :update-template :learn-from-result))

(in-package :bidirectional-llm/prompts)

(defclass prompt-template ()
  ((name :initarg :name :accessor template-name)
   (structure :initarg :structure :accessor template-structure)
   (success-rate :initform 0.0 :accessor template-success-rate)
   (usage-count :initform 0 :accessor template-usage-count)))

(defvar *prompt-templates* (make-hash-table :test #'equal))

(defun generate-prompt (context function-signature docstring)
  "Generate a contextual prompt for LLM code completion"
  (let ((template (gethash (infer-template-type context) *prompt-templates*)))
    (format nil "~A~%Context: ~A~%Signature: ~A~%Documentation: ~A~%Generate implementation:"
            (template-structure template)
            (serialize-context context)
            function-signature
            docstring)))

(defun update-template (template-name success-p feedback)
  "Update template based on generation results"
  (let ((template (gethash template-name *prompt-templates*)))
    (when template
      (incf (template-usage-count template))
      (setf (template-success-rate template)
            (/ (+ (* (template-success-rate template) 
                     (1- (template-usage-count template)))
                  (if success-p 1.0 0.0))
               (template-usage-count template)))
      (when feedback
        (adapt-template-structure template feedback)))))
