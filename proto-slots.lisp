
(defpackage #:proto-slots
  (:use :cl)
  (:export def-proto-slots
           get-strategy add-strategy remove-strategy
           simple unique-merge))

(in-package proto-slots)


(defparameter *strategies* (list)
  "An alist of proto-slots strategies: (name . func)
  name is the strategy name symbol, usually a keyword.
  func is a function name or a function object.
  NOTE: Data structure may change in a future version.")


(defun get-strategy (name)
  "Return the function associated with the given strategy name, or nil
  if that strategy is not registered."
  (cdr (assoc name *strategies*)))

(defun add-strategy (name func)
  "Register the given strategy name, using the given function name or
  function object. If the given strategy name is already registered,
  the existing registration is replaced."
  (setf *strategies*
        (cons (cons name func)
              (remove name *strategies* :key #'car)))
  (values))

(defun remove-strategy (name)
  "Unregister the given strategy name, if it is registered."
  (setq *strategies*
        (remove name *strategies* :key #'car))
  (values))


(defmacro def-proto-slots (class &body (slot-defs))
  "Define prototypal accessor methods on a class' slots.
  Each slot-def is a list like this:
    (slot-name strategy &rest strategy-keyword-args)
  Available strategies and their valid keyword args are:
    :simple
      :base :accessor :reader :own-reader :writer
    :unique-merge
      :base :accessor :reader :own-reader :writer
      :test :key :finder :own-finder :adder
  See the proto-slots README for details."
  `(progn
     ,@(mapcar
        (lambda (slot-def)
          (destructuring-bind (slot strategy &rest keys) slot-def
            (let ((func (get-strategy strategy)))
              (if func
                  (apply func class slot keys)
                  (error "Unknown proto-slot strategy ~S for slot ~S"
                         strategy slot)))))
        slot-defs)))


(defmacro %proto-method (name args docs &body body)
  "Define a generic function if it doesn't already exist, then define
  a method on that generic function. name is the function/method name.
  args are the method args, possibly including specializers. docs is
  the docstring for the generic function. body is the method body."
  (let ((simple-args
         (mapcar (lambda (arg) (if (consp arg) (car arg) arg))
                 args)))
    `(progn
       (unless (fboundp ',name)
         (defgeneric ,name ,simple-args
           (:documentation ,docs)))
       (defmethod ,name ,args ,@body))))


;;;;;;;;;;;;
;; SIMPLE ;;
;;;;;;;;;;;;

(add-strategy :simple 'simple)

(defun simple
    (class slot-name &key
     base (accessor slot-name) (reader accessor) own-reader
     (writer (and accessor (list 'setf accessor))))
  "Implements the :simple strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (list
   'progn
   (when own-reader
     `(%proto-method ,own-reader ((object ,class))
          ,(format nil "Returns the object's own ~(~A~), even if it is nil, ignoring inheritence." slot-name)
        (slot-value object ',slot-name)))

   (when reader
     `(%proto-method ,reader ((object ,class))
          ,(format nil "Returns the object's ~(~A~). If that is nil and the object has a base, returns the ~(~A~)'s ~(~0@*~A~) instead." slot-name base)
        (let ((val (and (slot-boundp object ',slot-name)
                        (slot-value object ',slot-name))))
          (if (,base object)
              (or val (,reader (,base object)))
              val))))

   (when writer
     `(%proto-method ,writer (value (object ,class))
          ,(format nil "Set the object's own ~(~A~)." reader)
        (setf (slot-value object ',slot-name) value)))))



;;;;;;;;;;;;;;;;;;
;; UNIQUE-MERGE ;;
;;;;;;;;;;;;;;;;;;

(add-strategy :unique-merge 'unique-merge)

(defun unique-merge
    (class slot-name &key
     base finder own-finder adder key test
     (accessor slot-name) (reader accessor) own-reader
     (writer (and accessor (list 'setf accessor))))
  "Implements the :unique-merge strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (list
   'progn

   (when own-reader
     `(%proto-method ,own-reader ((object ,class))
          ,(format nil "Return the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~)." reader base)
        (slot-value object ',slot-name)))

   (when reader
     `(%proto-method ,reader ((object ,class))
          ,(format nil "Return the object's ~(~A~) list, including any inherited from the object's ~(~A~)." reader base)
        (let ((b (,base object))
              (val (and (slot-boundp object ',slot-name)
                        (slot-value object ',slot-name))))
          (if b
              (union (,reader b) val :key ,key :test ,test)
              val))))

   (when writer
     `(%proto-method ,writer (value (object ,class))
          ,(format nil "Set the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~)." reader base)
        (setf (slot-value object ',slot-name) value)))

   (when own-finder
     `(%proto-method ,own-finder ((object ,class) query)
          ,(format nil "Find and return the first matching item in the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~). Return nil if there is no match." reader base)
        (if (slot-boundp object ',slot-name)
            (find query (slot-value object ',slot-name)
                  :key ,key :test ,test)
            nil)))

   (when finder
     `(%proto-method ,finder ((object ,class) query)
          ,(format nil "Find and return the first matching item from the object's ~(~A~) list, including those inherited from the object's ~(~A~). Return nil if there is no match." reader base)
        (or (and (slot-boundp object ',slot-name)
                 (find query (slot-value object ',slot-name)
                       :key ,key :test ,test))
            (,finder (,base object) query))))

   (when adder
     `(%proto-method ,adder ((object ,class) new-item)
          ,(format nil "Add the given item to the object's own ~(~A~) reader, NOT including those inherited from the object's ~(~A~). If the list already contains an item that matches the given item, the existing item will be removed before adding the given item." reader base)
        (setf (slot-value object ',slot-name)
              (union (and (slot-boundp object ',slot-name)
                          (slot-value object ',slot-name))
                     (list new-item)
                     :key ,key :test ,test))))))
