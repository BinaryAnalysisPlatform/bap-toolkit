

(defmethod jumping(_ _ )
  (when (is-restrictness-violation)
    (incident-report 'restrictness-violation (incident-location))))
