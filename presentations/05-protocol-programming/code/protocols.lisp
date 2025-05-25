(defpackage :protocol-programming
  (:use :cl)
  (:export :defprotocol :implement-protocol :protocol-implemented-p :require-protocol))

(in-package :protocol-programming)

(defmacro defprotocol (name &body method-specs)
  "Define a protocol as a set of generic function signatures"
  `(progn
     ;; Create protocol registry entry
     (defvar ,(intern (format nil "*~A-PROTOCOL*" name)) 
       (make-protocol ',name ',method-specs))
     
     ;; Define generic functions for each method
     ,@(mapcar (lambda (spec)
                 (destructuring-bind (method-name lambda-list &optional documentation)
                     spec
                   `(defgeneric ,method-name ,lambda-list
                      ,@(when documentation `((:documentation ,documentation))))))
               method-specs)
     
     ;; Create protocol checker
     (defun ,(intern (format nil "~A-PROTOCOL-P" name)) (object)
       ,(format nil "Check if object implements the ~A protocol" name)
       (protocol-implemented-p object ',name))
     
     ;; Export protocol name
     ',name))

(defstruct protocol
  name
  methods
  implementations)

(defvar *protocols* (make-hash-table))

(defun make-protocol (name method-specs)
  "Create a protocol definition"
  (let ((protocol (make-protocol :name name :methods method-specs)))
    (setf (gethash name *protocols*) protocol)
    protocol))

(defmacro implement-protocol (protocol-name type &body method-implementations)
  "Implement a protocol for a specific type"
  `(progn
     ,@(mapcar (lambda (impl)
                 (destructuring-bind (method-name args &body body) impl
                   `(defmethod ,method-name ,(cons `(,(first args) ,type) (rest args))
                      ,@body)))
               method-implementations)
     
     ;; Register implementation
     (register-protocol-implementation ',protocol-name ',type)
     
     ;; Verify implementation completeness
     (verify-protocol-implementation ',protocol-name ',type)))

(defun register-protocol-implementation (protocol-name type)
  "Register that a type implements a protocol"
  (let ((protocol (gethash protocol-name *protocols*)))
    (when protocol
      (pushnew type (protocol-implementations protocol)))))

(defun protocol-implemented-p (object protocol-name)
  "Check if an object's type implements a protocol"
  (let ((protocol (gethash protocol-name *protocols*))
        (object-type (type-of object)))
    (when protocol
      (member object-type (protocol-implementations protocol)))))

(defun verify-protocol-implementation (protocol-name type)
  "Verify that all protocol methods are implemented for a type"
  (let ((protocol (gethash protocol-name *protocols*)))
    (when protocol
      (dolist (method-spec (protocol-methods protocol))
        (let ((method-name (first method-spec))
              (lambda-list (second method-spec)))
          (unless (find-method (symbol-function method-name)
                               '() 
                               (cons type (rest lambda-list))
                               nil)
            (warn "Method ~A not implemented for type ~A in protocol ~A"
                  method-name type protocol-name)))))))

;; Example protocol definitions
(defprotocol serializable
  (serialize (object) "Convert object to serializable representation")
  (deserialize (data type) "Reconstruct object from serialized data"))

(defprotocol cacheable
  (cache-key (object) "Generate unique cache key for object")
  (cache-ttl (object) "Return time-to-live for cached object")
  (invalidate-cache (object) "Mark cached object as invalid"))

(defprotocol validatable
  (validate (object) "Validate object state, return T if valid")
  (validation-errors (object) "Return list of validation errors")
  (fix-validation-errors (object) "Attempt to fix validation errors"))
