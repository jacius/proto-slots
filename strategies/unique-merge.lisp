(in-package proto-slots)

(add-strategy :unique-merge 'unique-merge)

(defun unique-merge
    (class slot-name &key
     base (reader slot-name) finder own-finder adder key test)
  "Implements the :unique-merge strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (list
   'progn

   (when reader
     `(mkmethod ,reader ((object ,class))
          ,(format nil "Return the object's ~(~A~) list, including any inherited from the object's ~(~A~)." reader base)
        (let ((b (,base object))
              (val (slot-value-if-bound object ',slot-name)))
          (if b
              (union (,reader b) val :key ,key :test ,test)
              val))))

   (when own-finder
     `(mkmethod ,own-finder ((object ,class) query)
          ,(format nil "Find and return the first matching item in the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~). Return nil if there is no match." (or reader slot-name) base)
        (if (slot-boundp object ',slot-name)
            (find query (slot-value object ',slot-name)
                  :key ,key :test ,test)
            nil)))

   (when finder
     `(mkmethod ,finder ((object ,class) query)
          ,(format nil "Find and return the first matching item from the object's ~(~A~) list, including those inherited from the object's ~(~A~). Return nil if there is no match." (or reader slot-name) base)
        (or (and (slot-boundp object ',slot-name)
                 (find query (slot-value object ',slot-name)
                       :key ,key :test ,test))
            (,finder (,base object) query))))

   (when adder
     `(mkmethod ,adder ((object ,class) new-item)
          ,(format nil "Add the given item to the object's own ~(~A~) reader, NOT including those inherited from the object's ~(~A~). If the list already contains an item that matches the given item, the existing item will be removed before adding the given item." (or reader slot-name) base)
        (setf (slot-value object ',slot-name)
              (union (slot-value-if-bound object ',slot-name)
                     (list new-item)
                     :key ,key :test ,test))))))
