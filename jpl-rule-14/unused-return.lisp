
(defmethod read (var val)
  (let ((taint (taint-get-direct 'must-be-used val))
        (addr (get-current-program-counter))
        (first-time (not (dict-has 'was-read taint))))
    (when (and taint first-time)
      (dict-add 'was-read taint addr))))

(defmethod stored (addr val)
  (let ((taint (taint-get-direct 'must-be-used val))
        (prev (dict-get 'was-read taint))
        (addr (get-current-program-counter)))
    (when taint
      (when (= addr prev)
        (dict-del 'was-read taint)))))

(defmethod loaded (ptr val)
  (let ((taint (taint-get-direct 'must-be-used val)))
    (mark-used taint)
    (taint-sanitize-indirect 'must-be-used ptr)
    (taint-sanitize-direct   'must-be-used val)))

(defmethod taint-reached-finish (taint)
  (let ((read (dict-has 'was-read taint))
        (is-new (not (is-known-usage taint))))
    (when read
      (mark-used taint))
    (when (and (not read) is-new)
      (mark-unused taint)
      (notify-unused taint))))

(defun notify-unused (taint)
  (incident-report 'value-was-not-used (incident-location)))

(defmethod written (var val)
  (let ((name (dict-get 'call-return var)))
    (when name
      (let ((addr (get-current-program-counter))
            (old-taint (taint-get-direct 'must-be-used val)))
        (when old-taint
          (taint-sanitize-direct 'must-be-used val))
        (dict-del 'call-return var)
        (let ((taint (taint-introduce-directly 'must-be-used val)))
          (check-if-used taint name addr))))))

(defun is-ignored (name)
  (is-in name '__primus_linker_unresolved_call))

(defmethod call-return (name _ )
  (when (not (is-ignored name))
    (let ((addr (get-current-program-counter))
          (arg (return-arg name)))
      (dict-add 'call-return arg name))))
