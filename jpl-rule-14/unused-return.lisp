
(defmethod read (var val)
  (when (need-to-check val)
    (mark-used val)
    (dict-add 'addrs (get-current-program-counter) val)))

(defmethod stored (addr byte)
  (let ((pc (get-current-program-counter)))
    (when (dict-has 'addrs pc)
      (let ((value (dict-get 'addrs pc)))
        (mark-unused value)
        (dict-add 'values addr value)))))

(defmethod loaded (addr val)
  (when (dict-has 'values addr)
    (let ((value (dict-get 'values addr)))
      (mark-used value))))

(defmethod written (var val)
  (let ((name (return-from-sub val)))
    (when name
      (let ((addr (callsite-addr name))
            (loc  (get-location addr)))
        (when (and addr loc)
            (check-if-used val name addr))))))

(defmethod jumping (_ addr)
  (when (is-known-symbol addr)
    (save-location (get-current-program-counter) (incident-location))))

(defmethod notify-unused-return (addr)
  (incident-report 'unused-return-value (get-location addr)))
