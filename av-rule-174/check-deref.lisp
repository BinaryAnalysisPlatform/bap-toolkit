
(require taint)

(defun notify-null-deref ()
  (null-ptr-dereference)
  (incident-report 'null-pointer-dereference (incident-location)))

(defun check-deref-null-ptr (ptr)
  (when (and (not ptr) (all-static-constant ptr))
    (let ((taint (taint-get-direct 'const-ptr ptr))
          (checked (dict-has 'checked taint))
          (is-bad (taint-get-direct 'dont-believe ptr)))
      (when (not is-bad)
        (when (and taint (not checked))
          (notify-null-deref))
        (when (and taint checked)
          (let ((addr (dict-get 'checked taint))
                (dependent (has-control-dependency addr)))
            (when (not dependent)
              (notify-null-deref))))))))

(defmethod eval-cond (x)
  (let ((taint (taint-get-direct 'const-ptr x))
        (addr (get-current-program-counter)))
    (when taint
      (dict-add 'checked taint addr))))

(defmethod written (v x)
  (when (all-static-constant x)
    (taint-introduce-directly 'const-ptr x)))

(defmethod stored (a x)
  (when (all-static-constant x)
    (taint-introduce-directly 'const-ptr x)))

(defmethod call-return (name)
  (when (is-unresolved name)
    (let ((arg (abi-specific-return-arg)))
      (dict-add 'ignored-vars arg name))))

(defmethod read (var val)
  (when (dict-has 'ignored-vars var)
    (dict-del 'ignored-vars var)
    (when (taint-get-direct 'const-ptr val)
      (msg "discarded tainted value due to previous unresolved call")
      (taint-introduce-directly 'dont-believe val))))

(defmethod loading (ptr)
  (check-deref-null-ptr ptr))

(defmethod storing (ptr)
  (check-deref-null-ptr ptr))
