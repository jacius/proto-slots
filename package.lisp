
(defpackage #:proto-slots
  (:use :cl)
  (:export def-proto-slots mkmethod
           get-strategy add-strategy remove-strategy           
           simple unique-merge))