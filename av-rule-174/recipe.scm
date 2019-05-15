(parameter depth 8192 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")
(parameter incidents incidents "a path to file with incidents")

(option primus-lisp-load
        posix
        check-deref)

(option api-path $prefix/api)

(option run)
(option run-entry-points ${entry-points})
(option constant-tracker-enable)

(option primus-lisp-add $prefix)
(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)
(option primus-print-output $incidents)

(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout
  <stderr>:$prefix/stderr)

(option primus-print-observations
  pc-changed
  jumping
  call
  call-return
  machine-switch
  machine-fork
  lisp-message
  incident
  incident-location)
