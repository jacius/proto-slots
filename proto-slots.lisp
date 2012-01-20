
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
  See the proto-slots README for more information."
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


(defmacro mkmethod (name args docs &body body)
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


(declaim (inline slot-value-if-bound))
(defun slot-value-if-bound (object slot-name)
  "Return the slot value if the slot is bound, or nil otherwise."
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
      nil))
