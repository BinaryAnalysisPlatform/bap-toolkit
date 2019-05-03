
(defun release (val kind)
  (let ((taint (taint-get-direct kind val))
        (sub (dict-get 'tainted-locations taint)))
    (when taint
      (taint-sanitize-direct kind val)
      (when sub
        (dict-del 'tainted-locations taint)))))

(defun mark-as-read (v)
  (let ((taint (taint-get-direct 'must-be-used v)))
    (when taint
      (msg "marked as read $0" taint)
      (dict-add 'was-read taint v))))

(defmethod read (var val)
  (when (taint-get-direct 'must-be-used val)
    (mark-as-read val)))

(defmethod stored (var val)
  (let ((taint (taint-get-direct 'must-be-used val))
        (sub (dict-get 'tainted-locations taint))
        (callee (dict-get 'call-locations taint)))
    (when taint
      (let ((taint' (taint-introduce-directly 'must-be-loaded val)))
        (when sub
          (dict-add 'tainted-locations taint' sub))
        (when callee
          (dict-add 'call-locations taint' callee))))))

(defmethod loaded (_ val)
  (release val 'must-be-used)
  (release val 'must-be-loaded))

(defmethod taint-finalize (taint _)
  (let ((sub (dict-get 'tainted-locations taint))
        (read (dict-has 'was-read taint))
        (callee (dict-get 'call-site taint)))
    (when (and sub (not read))
      (notify-unused sub callee)
      (dict-del 'tainted-locations taint))))

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
      (dict-del 'tainted-locations old-taint)
      (dict-del 'call-site old-taint)
      (taint-sanitize-direct 'must-be-used val))
    (when name
      (let ((taint (taint-introduce-directly 'must-be-used val)))
        (dict-add 'tainted-locations taint name)
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
