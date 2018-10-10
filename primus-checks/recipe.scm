(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")

(option primus-lisp-load
  posix
  memcheck-malloc
  limit-malloc
  taint-sources
  sensitive-sinks
  warn-unused
  check-hardcoded-values
  check-null-pointers)

(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout)

(option report-progress)
(option log-dir log)

(option run)
(option run-entry-points ${entry-points})
(option constant-tracker-enable)

(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-print-output incidents)
(option primus-limit-max-length $depth)

(option primus-lisp-add $prefix)
(option primus-print-obs
  pc-changed
  jumping
  call
  call-return
  machine-switch
  machine-fork
  lisp-message
  incident
  incident-location)
