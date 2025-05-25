(defpackage :distributed-repl/telemetry
  (:use :cl)
  (:export :collect-system-metrics :track-function-calls :create-metric-dashboard))

(in-package :distributed-repl/telemetry)

(defclass metric-collector ()
  ((metrics :initform (make-hash-table :test #'equal) :accessor collector-metrics)
   (collection-interval :initarg :interval :accessor collection-interval :initform 1)
   (active-p :initform nil :accessor collector-active-p)))

(defvar *metric-collector* (make-instance 'metric-collector))

(defun start-metric-collection (&key (interval 1))
  "Start collecting system metrics at specified interval"
  (setf (collection-interval *metric-collector*) interval)
  (setf (collector-active-p *metric-collector*) t)
  (bordeaux-threads:make-thread
   (lambda ()
     (loop while (collector-active-p *metric-collector*)
           do (collect-system-metrics)
              (sleep interval)))
   :name "metric-collector"))

(defun collect-system-metrics ()
  "Collect current system metrics"
  (let ((timestamp (get-universal-time)))
    (setf (gethash timestamp (collector-metrics *metric-collector*))
          (list :memory-usage (get-memory-usage)
                :cpu-usage (get-cpu-usage)
                :thread-count (length (bordeaux-threads:all-threads))
                :heap-size (get-heap-size)
                :gc-stats (get-gc-statistics)))))

(defmacro with-telemetry (function-name &body body)
  "Wrap function execution with telemetry collection"
  `(let ((start-time (get-internal-real-time))
         (start-memory (get-memory-usage)))
     (unwind-protect
          (progn ,@body)
       (let ((end-time (get-internal-real-time))
             (end-memory (get-memory-usage)))
         (record-function-telemetry 
          ',function-name
          :duration (- end-time start-time)
          :memory-delta (- end-memory start-memory))))))

(defun create-real-time-dashboard ()
  "Generate HTML dashboard for real-time metrics"
  (format nil "
<html>
<head><title>Distributed Lisp System Dashboard</title></head>
<body>
<h1>System Health Dashboard</h1>
<div id='metrics'></div>
<script>
  function updateMetrics() {
    fetch('/api/metrics')
      .then(response => response.json())
      .then(data => {
        document.getElementById('metrics').innerHTML = formatMetrics(data);
      });
  }
  setInterval(updateMetrics, 1000);
</script>
</body>
</html>"))
