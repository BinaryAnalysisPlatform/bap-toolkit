;; when a call is made to a wur function, taint the result
(defmethod bap:warn-unused-result/introduce (taint)
  (dict-add 'warn-unused-results/taints
            taint
            (incident-location)))

;; or used in an external function
(defmethod bap:warn-unused-result/sanitize (taint)
  (dict-del 'warn-unused-result/taints taint))

;; when taint dies without being sanitized report an incident
(defmethod taint-finalize (taint live)
  (let ((unchecked (dict-get 'warn-unused-results/taints taint)))
    (when (and unchecked (not live))
      (incident-report 'warn-unused-result unchecked)
      (dict-del 'warn-unused-result/taints unchecked))))
