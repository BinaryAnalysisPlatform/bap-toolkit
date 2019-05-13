
(defun release (val kind)
  (let ((taint (taint-get-direct kind val))
        (sub (dict-get 'maybe-unused taint)))
    (when taint
      (taint-sanitize-direct kind val)
      (when sub
        (dict-del 'maybe-unused taint)))))

(defmethod read (var val)
  (let ((taint (taint-get-direct 'must-be-used val)))
    (when taint
      (dict-add 'was-read taint val))))

(defmethod stored (var val)
  (let ((taint  (taint-get-direct 'must-be-used val))
        (symbol (dict-get 'maybe-unused taint))
        (callee (dict-get 'call-site taint)))
    (when taint
      (let ((taint' (taint-introduce-directly 'must-be-loaded val)))
        (when symbol
          (dict-add 'maybe-unused taint' symbol))
        (when callee
          (dict-add 'call-site taint' callee))))))

(defmethod loaded (_ val)
  (release val 'must-be-used)
  (release val 'must-be-loaded))

(defmethod taint-finalize (taint _)
  (let ((sub (dict-get 'maybe-unused taint))
        (read (dict-has 'was-read taint))
        (callee (dict-get 'call-site taint)))
    (when (and sub read)
      (notify-used-result sub callee)
      (dict-del 'maybe-unused taint))
    (when (and sub (not read))
      (notify-unused sub callee)
      (dict-del 'maybe-unused taint))))

(defun notify-unused (sub addr)
  (notify-unused-result sub addr)
  (incident-report 'value-was-not-used sub))

(defun is-ignored (name)
  (is-in name
         '__primus_linker_unresolved_call))

(defmethod written (var val)
  (let ((name (dict-get 'call-return var))
        (addr (dict-get 'call-site name))
        (old-taint (taint-get-direct 'must-be-used val)))
    (when old-taint
      (dict-del 'maybe-unused old-taint)
      (dict-del 'call-site old-taint)
      (taint-sanitize-direct 'must-be-used val))
    (when name
      (let ((taint (taint-introduce-directly 'must-be-used val)))
        (dict-add 'maybe-unused taint name)
        (dict-add 'call-site taint addr)))))

(defmethod jumping (_ _)
  (stack-push 'callers (get-current-program-counter)))

(defmethod call (name _ )
  (let ((caller (stack-pop 'callers)))
    (when (not (is-ignored name))
      (let ((arg (return-arg name)))
        (dict-add 'call-return arg name)
        (when caller
          (dict-add 'call-site name caller))))))
