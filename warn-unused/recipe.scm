(parameter depth 4096 "a depth of analysis")
(parameter entry-points all-subroutines "where to search")

(option primus-lisp-load
        warn-unused-result
        posix)

(option passes
        with-no-return
        callsites
        run)

(option warn-unused-result-enable)

(option primus-promiscuous-mode)
(option primus-greedy-scheduler)
(option primus-limit-max-length $depth)

(option primus-lisp-add $prefix)
(option primus-print-output incidents)
(option run-entry-points ${entry-points})

(option primus-print-observations
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

(option report-progress)
(option log-dir log)
