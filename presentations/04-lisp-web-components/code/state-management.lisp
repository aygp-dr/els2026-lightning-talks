(defpackage :lisp-web-components/state
  (:use :cl)
  (:export :create-store :subscribe :dispatch :get-state :update-state))

(in-package :lisp-web-components/state)

(defclass app-store ()
  ((state :initform (make-hash-table :test #'equal) :accessor store-state)
   (subscribers :initform '() :accessor store-subscribers)
   (middleware :initform '() :accessor store-middleware)))

(defvar *global-store* nil)

(defun create-store (&optional initial-state)
  "Create a new application store"
  (let ((store (make-instance 'app-store)))
    (when initial-state
      (setf (store-state store) 
            (alexandria:alist-hash-table initial-state :test #'equal)))
    (setf *global-store* store)
    store))

(defun get-state (&optional (store *global-store*) path)
  "Get state value, optionally at a specific path"
  (if path
      (reduce (lambda (state key)
                (if (hash-table-p state)
                    (gethash key state)
                    (getf state key)))
              (if (listp path) path (list path))
              :initial-value (store-state store))
      (alexandria:hash-table-alist (store-state store))))

(defun update-state (updates &optional (store *global-store*))
  "Update store state and notify subscribers"
  (let ((old-state (alexandria:copy-hash-table (store-state store))))
    ;; Apply updates
    (loop for (path value) in updates
          do (set-nested-value (store-state store) path value))
    
    ;; Notify subscribers
    (dolist (subscriber (store-subscribers store))
      (funcall subscriber old-state (store-state store)))
    
    (store-state store)))

(defun subscribe (callback &optional (store *global-store*))
  "Subscribe to state changes"
  (push callback (store-subscribers store))
  ;; Return unsubscribe function
  (lambda ()
    (setf (store-subscribers store)
          (remove callback (store-subscribers store)))))

(defun dispatch (action &optional (store *global-store*))
  "Dispatch an action to update state"
  (let ((action-type (getf action :type))
        (payload (getf action :payload)))
    (case action-type
      (:add-todo
       (let ((new-todo (list :id (generate-id)
                             :text (getf payload :text)
                             :completed-p nil
                             :created-at (get-universal-time))))
         (update-state `(("todos" ,(append (get-state store "todos") 
                                          (list new-todo)))))))
      (:toggle-todo
       (let ((todo-id (getf payload :id)))
         (update-state 
          `(("todos" ,(mapcar (lambda (todo)
                                (if (equal (getf todo :id) todo-id)
                                    (list* :completed-p (not (getf todo :completed-p))
                                           (alexandria:remove-from-plist todo :completed-p))
                                    todo))
                              (get-state store "todos")))))))
      (:delete-todo
       (let ((todo-id (getf payload :id)))
         (update-state 
          `(("todos" ,(remove-if (lambda (todo)
                                   (equal (getf todo :id) todo-id))
                                 (get-state store "todos"))))))))))

;; Utility functions
(defun generate-id ()
  "Generate a unique ID"
  (format nil "~A-~A" (get-universal-time) (random 1000)))

(defun set-nested-value (hash-table path value)
  "Set a value at a nested path in a hash table"
  (if (= (length path) 1)
      (setf (gethash (first path) hash-table) value)
      (let ((nested (gethash (first path) hash-table)))
        (unless nested
          (setf nested (make-hash-table :test #'equal))
          (setf (gethash (first path) hash-table) nested))
        (set-nested-value nested (rest path) value))))
