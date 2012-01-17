(in-package proto-slots)

(add-strategy :simple 'simple)

(defun simple
    (class slot-name &key base (reader slot-name))
  "Implements the :simple strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (list
   'progn
   (when reader
     `(mkmethod ,reader ((object ,class))
          ,(format nil "Returns the object's ~(~A~). If that is nil and the object has a base, returns the ~(~A~)'s ~(~0@*~A~) instead." reader base)
        (let ((val (slot-value-if-bound object ',slot-name)))
          (if (,base object)
              (or val (,reader (,base object)))
              val))))))
