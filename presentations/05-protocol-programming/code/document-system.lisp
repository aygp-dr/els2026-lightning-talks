(defpackage :document-system
  (:use :cl :protocol-programming)
  (:export :document :pdf-document :markdown-document :image-document))

(in-package :document-system)

;; Base document structure
(defclass document ()
  ((title :initarg :title :accessor document-title)
   (author :initarg :author :accessor document-author)
   (created-date :initarg :created-date :accessor document-created-date)
   (content :initarg :content :accessor document-content)))

;; Specific document types
(defclass pdf-document (document)
  ((page-count :initarg :page-count :accessor pdf-page-count)
   (file-path :initarg :file-path :accessor pdf-file-path)))

(defclass markdown-document (document)
  ((raw-markdown :initarg :raw-markdown :accessor markdown-raw-content)
   (rendered-html :initarg :rendered-html :accessor markdown-rendered-html)))

(defclass image-document (document)
  ((width :initarg :width :accessor image-width)
   (height :initarg :height :accessor image-height)
   (format :initarg :format :accessor image-format)))

;; Implement serializable protocol for different document types
(implement-protocol serializable pdf-document
  (serialize (doc)
    `(:type :pdf
      :title ,(document-title doc)
      :author ,(document-author doc)
      :created-date ,(document-created-date doc)
      :page-count ,(pdf-page-count doc)
      :file-path ,(pdf-file-path doc)))
  
  (deserialize (data type)
    (make-instance 'pdf-document
                   :title (getf data :title)
                   :author (getf data :author)
                   :created-date (getf data :created-date)
                   :page-count (getf data :page-count)
                   :file-path (getf data :file-path))))

(implement-protocol serializable markdown-document
  (serialize (doc)
    `(:type :markdown
      :title ,(document-title doc)
      :author ,(document-author doc)
      :created-date ,(document-created-date doc)
      :raw-markdown ,(markdown-raw-content doc)))
  
  (deserialize (data type)
    (let ((doc (make-instance 'markdown-document
                              :title (getf data :title)
                              :author (getf data :author)
                              :created-date (getf data :created-date)
                              :raw-markdown (getf data :raw-markdown))))
      ;; Render markdown to HTML
      (setf (markdown-rendered-html doc) 
            (render-markdown-to-html (getf data :raw-markdown)))
      doc)))

;; Implement cacheable protocol
(implement-protocol cacheable pdf-document
  (cache-key (doc)
    (format nil "pdf:~A:~A" (document-title doc) (pdf-file-path doc)))
  
  (cache-ttl (doc)
    (* 24 60 60)) ; 24 hours
  
  (invalidate-cache (doc)
    (remove-from-cache (cache-key doc))))

(implement-protocol cacheable markdown-document
  (cache-key (doc)
    (format nil "md:~A:~A" (document-title doc) 
            (sxhash (markdown-raw-content doc))))
  
  (cache-ttl (doc)
    (* 60 60)) ; 1 hour (shorter for dynamic content)
  
  (invalidate-cache (doc)
    (remove-from-cache (cache-key doc))))

;; Implement validatable protocol
(implement-protocol validatable document
  (validate (doc)
    (null (validation-errors doc)))
  
  (validation-errors (doc)
    (let ((errors '()))
      (unless (document-title doc)
        (push "Document must have a title" errors))
      (unless (document-author doc)
        (push "Document must have an author" errors))
      (unless (document-created-date doc)
        (push "Document must have a creation date" errors))
      errors))
  
  (fix-validation-errors (doc)
    (unless (document-title doc)
      (setf (document-title doc) "Untitled Document"))
    (unless (document-author doc)
      (setf (document-author doc) "Unknown Author"))
    (unless (document-created-date doc)
      (setf (document-created-date doc) (get-universal-time)))
    doc))

;; Protocol-based document processing
(defun process-documents (documents)
  "Process a list of documents using their implemented protocols"
  (dolist (doc documents)
    ;; Validate all documents
    (when (validatable-protocol-p doc)
      (unless (validate doc)
        (format t "Fixing validation errors for ~A~%" (document-title doc))
        (fix-validation-errors doc)))
    
    ;; Cache serializable documents
    (when (and (serializable-protocol-p doc)
               (cacheable-protocol-p doc))
      (let ((cache-key (cache-key doc))
            (serialized-data (serialize doc)))
        (store-in-cache cache-key serialized-data (cache-ttl doc))
        (format t "Cached ~A with key ~A~%" (document-title doc) cache-key)))
    
    ;; Additional processing based on available protocols
    (format t "Processed ~A (~A)~%" 
            (document-title doc) 
            (type-of doc))))

;; Utility functions
(defvar *cache* (make-hash-table :test #'equal))

(defun store-in-cache (key data ttl)
  "Store data in cache with TTL"
  (setf (gethash key *cache*) 
        (list :data data :expires (+ (get-universal-time) ttl))))

(defun remove-from-cache (key)
  "Remove item from cache"
  (remhash key *cache*))

(defun render-markdown-to-html (markdown)
  "Placeholder for markdown rendering"
  (format nil "<p>~A</p>" markdown))
