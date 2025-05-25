(defpackage :incremental-types/repl
  (:use :cl :incremental-types)
  (:export :enable-type-tracing :disable-type-tracing :*trace-types*))

(in-package :incremental-types/repl)

(defvar *trace-types* nil)
(defvar *traced-functions* '())

(defun enable-type-tracing ()
  "Enable automatic type tracing in the REPL"
  (setf *trace-types* t)
  (advice-add 'eval :around #'trace-eval-types)
  (format t "Type tracing enabled~%"))

(defun disable-type-tracing ()
  "Disable automatic type tracing"
  (setf *trace-types* nil)
  (advice-remove 'eval #'trace-eval-types)
  (format t "Type tracing disabled~%"))

(defun trace-eval-types (original-eval form &optional env)
  "Trace types during REPL evaluation"
  (when *trace-types*
    (let ((result (funcall original-eval form env)))
      (when (and (consp form) (symbolp (car form)))
        (let ((function-name (car form))
              (args (cdr form)))
          ;; Register argument types
          (loop for arg in args
                for i from 0
                do (register-observation 
                    (intern (format nil "~A-ARG-~A" function-name i) :keyword)
                    (eval arg env)
                    :repl-execution))
          ;; Register return type
          (register-observation 
           (intern (format nil "~A-RETURN" function-name) :keyword)
           result
           :repl-execution)))
      result)))

(defmacro with-type-learning (&body body)
  "Execute body with type learning enabled"
  `(let ((*trace-types* t))
     (enable-type-tracing)
     (unwind-protect
          (progn ,@body)
       (disable-type-tracing))))

;; Example usage
(defun suggest-type-declarations (function-name)
  "Suggest type declarations based on learned types"
  (let ((arg-types '())
        (return-type nil))
    (loop for i from 0
          for arg-key = (intern (format nil "~A-ARG-~A" function-name i) :keyword)
          for type-info = (gethash arg-key (function-types *type-db*))
          while type-info
          do (push (type-info-inferred-type type-info) arg-types))
    
    (let ((return-key (intern (format nil "~A-RETURN" function-name) :keyword)))
      (when-let ((return-info (gethash return-key (function-types *type-db*))))
        (setf return-type (type-info-inferred-type return-info))))
    
    (when (or arg-types return-type)
      `(declaim (ftype (function ,(reverse arg-types) 
                                 ,(or return-type t)) 
                       ,function-name)))))
