(parameter depth 32768 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")
(parameter verbosity 1 "the level of verbosity")

(option primus-lisp-load
  posix
  must-check-value)

(option primus-lisp-add $prefix)

(option must-check-value-enable)

(option passes
        with-no-return
        run)

(option primus-lisp-channel-redirect
  <stdin>:$prefix/stdin
  <stdout>:$prefix/stdout)

(option report-progress)
(option log-dir log)

(option run-entry-points ${entry-points})

(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-print-output incidents)
(option primus-limit-max-length $depth)

(option primus-taint-gc conservative)

(option must-check-value-verbose $verbosity)

(option primus-print-obs
  taint-finalize
  exception
  pc-changed
  jumping
  call
  call-return
  machine-switch
  machine-fork
  lisp-message
  incident
  incident-location)
