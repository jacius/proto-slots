(in-package proto-slots)

(add-strategy :simple 'simple)

(defun simple
    (class slot-name &key base (reader slot-name))
  "Implements the :simple strategy for def-proto-slots.

  The slot holds any value. When it is nil, the value is inherited
  from the base object. This strategy accepts the following keyword
  arguments:

  * :base - Func to get the base object. Required!
  * :reader - Func name to get slot value, with inheritance."

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
