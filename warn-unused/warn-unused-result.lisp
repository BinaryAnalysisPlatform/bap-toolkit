
(defmethod written (var val)
  (when (has-attr var 'warn-unused)
    (let ((taint (taint-introduce-directly 'warn-unused val)))
      (dict-add 'warn-unused-results/taints taint (incident-location))
      (check-if-used taint))))

(defmethod eval-cond (cnd)
  (let ((taint (taint-get-direct 'warn-unused cnd)))
    (when taint
      (dict-del 'warn-unused-results/taints taint)
      (taint-sanitize-direct 'warn-unused cnd))))

(defmethod read (var val)
  (when (is-external-argument var)
    (let ((taint (taint-get-direct 'warn-unused val)))
      (when taint
        (taint-sanitize-direct 'warn-unused val)
        (dict-del 'warn-unused-results/taints taint)))))

(defmethod taint-finalize (taint live)
  (let ((unchecked (dict-get 'warn-unused-results/taints taint)))
    (when unchecked
      (incident-report 'warn-unused-result unchecked)
      (notify-warn-unused-result taint)
      (dict-del 'warn-unused-result/taints unchecked))))
