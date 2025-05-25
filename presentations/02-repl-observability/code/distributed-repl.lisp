(defpackage :distributed-repl
  (:use :cl :usocket :bordeaux-threads)
  (:export :start-repl-server :connect-to-cluster :execute-on-nodes))

(in-package :distributed-repl)

(defclass distributed-repl-server ()
  ((port :initarg :port :accessor server-port :initform 4005)
   (nodes :initform (make-hash-table :test #'equal) :accessor server-nodes)
   (command-history :initform '() :accessor server-history)
   (active-sessions :initform '() :accessor server-sessions)))

(defvar *repl-server* nil)

(defun start-repl-server (&key (port 4005))
  "Start the distributed REPL server"
  (setf *repl-server* (make-instance 'distributed-repl-server :port port))
  (let ((server-socket (socket-listen *wildcard-host* port :reuse-address t)))
    (format t "Distributed REPL server started on port ~A~%" port)
    (make-thread 
     (lambda ()
       (loop
         (let ((client-socket (socket-accept server-socket)))
           (make-thread 
            (lambda () (handle-client-connection client-socket))
            :name "repl-client-handler"))))
     :name "repl-server")))

(defun register-node (node-id host port capabilities)
  "Register a new node in the distributed system"
  (setf (gethash node-id (server-nodes *repl-server*))
        (list :host host :port port :capabilities capabilities
              :status :connected :last-seen (get-universal-time))))

(defun execute-on-nodes (expression &key nodes (timeout 30))
  "Execute expression on specified nodes (or all if none specified)"
  (let ((target-nodes (or nodes (hash-table-keys (server-nodes *repl-server*)))))
    (mapcar (lambda (node-id)
              (execute-on-single-node node-id expression timeout))
            target-nodes)))

(defun execute-on-single-node (node-id expression timeout)
  "Execute expression on a single node with timeout"
  (let ((node-info (gethash node-id (server-nodes *repl-server*))))
    (when node-info
      (handler-case
          (with-timeout (timeout)
            (let ((result (send-command-to-node node-info expression)))
              (list :node node-id :status :success :result result)))
        (timeout-error ()
          (list :node node-id :status :timeout))
        (error (e)
          (list :node node-id :status :error :error (princ-to-string e)))))))
