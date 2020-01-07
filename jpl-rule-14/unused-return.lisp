
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
      (incident-report 'unused-return-value (dict-get 'unused-return-value taint)))))

(defmethod written (var val)
  (let ((name (return-from-sub val)))
    (when name
      (let ((addr (callsite-addr name))
            (loc (dict-get 'callsites addr))
            (old-taint (taint-get-direct 'must-be-used val)))
        (when (and addr loc)
          (when old-taint
            (taint-sanitize-direct 'must-be-used val))
          (dict-del 'call-return var)
          (let ((taint (taint-introduce-directly 'must-be-used val)))
            (dict-add 'unused-return-value taint loc)
            (check-if-used taint name addr)))))))

(defmethod jumping (_ addr)
  (when (is-known-symbol addr)
    (dict-add 'callsites (get-current-program-counter) (incident-location))))
