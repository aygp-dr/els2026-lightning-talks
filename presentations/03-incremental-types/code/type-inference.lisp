(defpackage :incremental-types
  (:use :cl)
  (:export :infer-types :register-observation :get-type-info :suggest-type-annotation))

(in-package :incremental-types)

(defclass type-database ()
  ((function-types :initform (make-hash-table) :accessor function-types)
   (variable-types :initform (make-hash-table) :accessor variable-types)
   (observations :initform '() :accessor type-observations)
   (confidence-scores :initform (make-hash-table) :accessor confidence-scores)))

(defvar *type-db* (make-instance 'type-database))

(defstruct type-info
  inferred-type
  confidence
  observations
  sources
  last-updated)

(defun register-observation (symbol value source)
  "Register a type observation from runtime or static analysis"
  (let ((observed-type (determine-type value))
        (existing-info (gethash symbol (function-types *type-db*))))
    (if existing-info
        (update-type-info existing-info observed-type source)
        (setf (gethash symbol (function-types *type-db*))
              (make-type-info 
               :inferred-type observed-type
               :confidence 0.1
               :observations (list value)
               :sources (list source)
               :last-updated (get-universal-time))))))

(defun determine-type (value)
  "Determine the most specific type for a value"
  (typecase value
    (integer (cond 
               ((typep value 'fixnum) 'fixnum)
               ((>= value 0) '(integer 0 *))
               (t 'integer)))
    (float (if (typep value 'single-float) 'single-float 'double-float))
    (string (if (= (length value) 0) '(string 0) `(string ,(length value))))
    (list (if (null value) 'null `(list ,@(mapcar #'determine-type value))))
    (vector `(vector ,(if (> (length value) 0) 
                          (determine-type (aref value 0)) 
                          t)
                     ,(length value)))
    (t (type-of value))))

(defun update-type-info (info new-type source)
  "Update existing type information with new observation"
  (push new-type (type-info-observations info))
  (push source (type-info-sources info))
  (setf (type-info-last-updated info) (get-universal-time))
  
  ;; Update confidence based on consistency
  (let ((consistent-observations 
          (count new-type (type-info-observations info) :test #'type-compatible-p)))
    (setf (type-info-confidence info)
          (min 1.0 (/ consistent-observations 
                      (length (type-info-observations info))))))
  
  ;; Update inferred type to most specific common type
  (setf (type-info-inferred-type info)
        (find-common-type (type-info-observations info))))

(defun type-compatible-p (type1 type2)
  "Check if two types are compatible"
  (or (equal type1 type2)
      (subtypep type1 type2)
      (subtypep type2 type1)))

(defun find-common-type (type-list)
  "Find the most specific type that encompasses all observed types"
  (reduce (lambda (acc type)
            (cond
              ((equal acc type) acc)
              ((subtypep type acc) acc)
              ((subtypep acc type) type)
              (t t))) ; Fall back to T if no common specific type
          type-list
          :initial-value (first type-list)))
