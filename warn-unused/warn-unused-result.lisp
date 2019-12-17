
(defmethod written (var val)
  (when (has-attr var 'warn-unused)
    (let ((taint (taint-introduce-directly 'warn-unused val)))
      (check-if-used taint))))

(defmethod jumping (cnd _)
  (let ((taint (taint-get-direct 'warn-unused cnd)))
    (when taint
      (taint-sanitize-direct 'warn-unused cnd)
      (mark-as-used taint))))

(defmethod read (var val)
  (when (is-external-argument var)
    (let ((taint (taint-get-direct 'warn-unused val)))
      (when taint
        (taint-sanitize-direct 'warn-unused val)
        (mark-as-used taint)))))
