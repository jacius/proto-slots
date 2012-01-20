
(defpackage #:proto-slots
  (:use :cl)
  (:export def-proto-slots mkmethod
           all-strategies get-strategy add-strategy remove-strategy
           simple unique-merge hash-merge))
