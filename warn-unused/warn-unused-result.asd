(defsystem bap:warn-unused-result
  :description "Checks that values, returned by functions that are
  annotated with the warn-unused-result attribute, are properly used."
  :depends-on (bap:taint-analyzer)
  :components (bap:warn-unused-tracker
               bap:conservative-garbage-collector))
