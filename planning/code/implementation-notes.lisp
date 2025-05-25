;; Development tracking
(defvar *talk-progress* 
  '((:bidirectional-llm 
     :concept-complete 70
     :implementation 40
     :demo 20
     :presentation 0)
    (:repl-observability
     :concept-complete 60
     :implementation 30
     :demo 10
     :presentation 0)
    (:incremental-types
     :concept-complete 50
     :implementation 20
     :demo 0
     :presentation 5)
    (:web-components
     :concept-complete 40
     :implementation 15
     :demo 5
     :presentation 0)
    (:protocol-programming
     :concept-complete 80
     :implementation 60
     :demo 30
     :presentation 10)))

(defun calculate-readiness (talk-id)
  "Calculate overall readiness score for a talk"
  (let ((progress (getf *talk-progress* talk-id)))
    (when progress
      (/ (+ (getf progress :concept-complete)
            (getf progress :implementation)
            (getf progress :demo)
            (getf progress :presentation))
         4))))

;; Example: (calculate-readiness :bidirectional-llm) => 32.5
