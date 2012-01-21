(in-package proto-slots)

(add-strategy :hash-merge 'hash-merge)


(defun merge-hash-tables
    (tables-list
     &key (test (hash-table-test (car (last tables-list)))))
  "Merge the list of hash tables into one new table. Later tables take
  precedence over earlier tables when there are key collisions. The
  test parameter specifies the test function for the new table."
  (let ((result (make-hash-table :test test)))
    (dolist (table tables-list)
      (loop for k being the hash-keys in table using (hash-value v)
         do (setf (gethash k result) v)))
    result))


(defun hash-merge
    (class slot-name &key
     base (reader slot-name) getter setter remover)
  "Implements the :hash-merge strategy for def-proto-slots.

  The slot holds a hash table, which inherits entries for any keys it
  doesn't already have. This strategy accepts the following keyword
  arguments:

  * :base - Func to get the base object. Required!
  * :reader - Func name to return the hash table, with inheritance.
  * :getter - Func name to look up a key's value, with inheritance.
  * :setter - Func name to set a key's value."

  (unless base
    (error "The :base keyword argument is required."))

  (list
   'progn

   (when reader
     `(mkmethod ,reader ((object ,class))
          ,(format nil "Return the object's ~(~A~) hash table, including any inherited from the object's ~(~A~)." reader base)
        (let ((b (,base object))
              (val (slot-value-if-bound object ',slot-name)))
          (if b
              (merge-hash-tables (list (,reader b) val))
              val))))

   (when getter
     `(mkmethod ,getter (key (object ,class))
          ,(format nil "Return the value for the given key in the object's ~(~A~) hash table, possibly inherited from the object's ~(~A~). The second return value is the object where the key was found (the original object, or its ~(~1@*~A~), etc.) or nil if the key was not found." (or reader slot-name) base)
        (if (slot-value-if-bound object ',slot-name)
            (multiple-value-bind (result memberp)
                (gethash key (slot-value object ',slot-name))
              (if (or memberp (null (,base object)))
                  (values result (and memberp object))
                  (,getter (,base object) key)))
            (if (,base object)
                (,getter (,base object) key)
                (values nil nil)))))

   (when setter
     (let ((args (if (and (consp setter) (eq 'setf (car setter)))
                     `(value (object ,class) key)
                     `(key (object ,class) value))))
       `(mkmethod ,setter ,args
            ,(format nil "Set the value of the given key in the object's own ~(~A~) hash table, obscuring the value inherited from the object's ~(~A~)." (or reader slot-name) base)
          (setf (gethash key (slot-value object ',slot-name)) value))))

   (when remover
     `(mkmethod ,remover ((object ,class) key)
          ,(format nil "Remove the given key from the object's own ~(~A~) hash table, exposing the value inherited from the object's ~(~A~)." (or reader slot-name) base)
        (remhash key (slot-value object ',slot-name))))))
