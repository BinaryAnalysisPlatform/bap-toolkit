(require memcheck)

(defmethod call (name ptr)
  (when (and ptr (= name 'free)
             (not (= ptr *malloc-zero-sentinel*)))
    (msg "memcheck-release")
    (memcheck-release 'alloc ptr)))

(defmethod call-return (name len ptr)
  (when (and len ptr (= name 'malloc))
    (msg "memcheck-acquire")
    (memcheck-acquire 'alloc ptr len)))

(defmethod call-return (name _ len ptr)
  (when (and len ptr (= name 'realloc))
    (msg "memcheck-acquire")
    (memcheck-acquire 'alloc ptr len)))
