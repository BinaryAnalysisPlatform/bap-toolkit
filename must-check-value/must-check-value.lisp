(require incident)

(defun value-must-be-checked (name v)
  (let ((pc (dict-get 'caller name))
        (loc (dict-get 'caller/location name))
        (tid (taint-introduce-directly 'value-check/required v)))
    (when pc
      (dict-add 'value-check/required tid pc))
    (dict-add 'value-check/location tid loc)))

;; infer an address of a callsite
(defun update-callee (addr)
  (let ((pc (get-current-program-counter)))
    (dict-add 'callee addr pc)
    (dict-add 'callee/location addr (incident-location))))

(defun update-caller (name)
  (let ((pc (get-current-program-counter))
        (called (dict-get 'callee pc))
        (called-loc (dict-get 'callee/location pc)))
    (when called
      (dict-add 'caller name called)
      (dict-add 'caller/location name called-loc))))

(defmethod call (name _)
  (update-caller name))

(defmethod eval-cond(cnd)
  (let ((taint (taint-get-direct 'value-check/required cnd))
        (loc (dict-get 'value-check/location taint))
        (pc (dict-get 'value-check/required taint)))
    (when taint
      (dict-add 'value-check/done taint taint)
      (taint-sanitize-direct 'value-check/required cnd))
    (when pc
      (dict-del 'check-value/required taint)
      (dict-del 'check-value/location taint))))

(defmethod jumping (cnd dst)
  (update-callee dst))

(defmethod taint-finalize (taint live)
  (let ((pc (dict-get 'value-check/required taint))
        (loc (dict-get 'value-check/location taint))
        (checked (dict-get 'value-check/done taint)))
    (when (and (not checked) pc)
      (incident-report 'value-was-not-checked loc)
      (notify-unchecked-value pc)
      (dict-del 'value-check/required taint))))

(defmethod call-return (name _ ret)
  (when (is-in name
           'chdir
           'malloc 'getenv 'mkdtemp 'mkstemp
           'posix_openpt 'system 'gets 'fgets 'feof
           'ferror 'fileno 'chdir 'pipe 'setuid)
    (value-must-be-checked name ret)))

(defmethod call-return (name _ _ ret)
  (when (is-in name 'calloc 'realoc 'fdopen 'fopen
               'realpath 'listen 'shutdown 'access 'getcwd
               'symlink 'truncate 'popen)
    (value-must-be-checked name ret)))

(defmethod call-return (name _ _ _ ret)
  (when (is-in name
               'freopen 'strxfrm 'accept 'bind 'connect
               'recvmsg 'sendmsg 'socket 'chown 'readv
               'readlink 'write)
    (value-must-be-checked name ret)))

(defmethod call-return (name _ _ _ _ ret)
  (when (is-in name 'fread 'fwrite 'recv 'send 'socketpair
               'pread 'pwrite 'read)
    (value-must-be-checked name ret)))

(defmethod call-return (name _ _ _ _ _ _ _ ret)
  (when (is-in name 'recvfrom 'sendto 'setsockopt)
    (value-must-be-checked name ret)))


(defmethod call-return (name _ _ _ _ _ _ ret)
  (when (is-in name 'recvfrom 'sendto)
    (value-must-be-checked name ret)))
