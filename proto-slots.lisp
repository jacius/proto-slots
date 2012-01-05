
(defpackage #:proto-slots
  (:use :cl)
  (:export def-proto-slots))

(in-package proto-slots)


;; Borrowed/adapted from alexandria.
(declaim (inline format-symbol))
(defun format-symbol (package control &rest arguments)
  "Constructs a string by applying ARGUMENTS to string designator
CONTROL as if by FORMAT, and then creates a symbol named by that
string. If PACKAGE is NIL, returns an uninterned symbol, if package is
T, returns a symbol interned in the current package, and otherwise
returns a symbol interned in the package designated by PACKAGE."
  (let ((name (apply #'format nil (string control) arguments)))
    (values
     (if package
         (intern name (if (eq t package) *package* package))
         (make-symbol name)))))


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

(defun %proto-slot/simple
    (class slot-name &key
     base (accessor slot-name) (reader accessor)
     (writer (and accessor (list 'setf accessor))))
  "Implements the :simple strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (let ((own (when reader 
               (format-symbol (symbol-package reader)
                              "OWN-~A" (symbol-name reader)))))
    (list
     'progn
     (when own
       `(%proto-method ,own ((object ,class))
            ,(format nil "Returns the object's own ~(~A~), even if it is nil, ignoring inheritence." slot-name)
          (slot-value object ',slot-name)))

     (when reader
       `(%proto-method ,reader ((object ,class))
            ,(format nil "Returns the object's ~(~A~). If that is nil and the object has a base, returns the ~(~A~)'s ~(~0@*~A~) instead." slot-name base)
          (if (,base object)
              (or (slot-value object ',slot-name)
                  (,reader (,base object)))
              (slot-value object ',slot-name))))

     (when writer
       `(%proto-method ,writer (value (object ,class))
            ,(format nil "Set the object's own ~(~A~)." reader)
          (setf (slot-value object ',slot-name) value))))))



;;;;;;;;;;;;;;;;;;
;; UNIQUE-MERGE ;;
;;;;;;;;;;;;;;;;;;

(defun %proto-slot/unique-merge
    (class slot-name &key
     base finder adder key test
     (accessor slot-name) (reader accessor)
     (writer (and accessor (list 'setf accessor))))
  "Implements the :unique-merge strategy for def-proto-slots."

  (unless base
    (error "The :base keyword argument is required."))

  (let ((own-reader (format-symbol (symbol-package reader)
                                   "OWN-~A" (symbol-name reader)))
        (own-finder (when finder
                      (format-symbol (symbol-package reader)
                                     "OWN-~A" (symbol-name finder)))))
    (list
     'progn

     (when own-reader
       `(%proto-method ,own-reader ((object ,class))
            ,(format nil "Return the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~)." reader base)
          (slot-value object ',slot-name)))

     (when reader
       `(%proto-method ,reader ((object ,class))
            ,(format nil "Return the object's ~(~A~) list, including any inherited from the object's ~(~A~)." reader base)
          (let ((b (,base object)))
            (if b (union (,reader b) (,own-reader object)
                         :key ,key :test ,test)
                (,own-reader object)))))

     (when writer
       `(%proto-method ,writer (value (object ,class))
            ,(format nil "Set the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~)." reader base)
          (setf (slot-value object ',slot-name) value)))

     (when own-finder
       `(%proto-method ,own-finder ((object ,class) name)
            ,(format nil "Find and return the first matching item in the object's own ~(~A~) list, NOT including those inherited from the object's ~(~A~). Return nil if there is no match." reader base)
          (find name (,own-reader object)
                :key ,key :test ,test)))

     (when finder
       `(%proto-method ,finder ((object ,class) name)
            ,(format nil "Find and return the first matching item from the object's ~(~A~) list, including those inherited from the object's ~(~A~). Return nil if there is no match." reader base)
          (find name (,reader object)
                :key ,key :test ,test)))

     (when adder
       `(%proto-method ,adder ((object ,class) item)
          ,(format nil "Add the given item to the object's own ~(~A~) reader, NOT including those inherited from the object's ~(~A~). If the list already contains an item that matches the given item, the existing item will be removed before adding the given item." reader base)
          (funcall #',writer
                   (union (,own-reader object) (list item)
                          :key ,key :test ,test)
                   object))))))


;;;;;;;;;;
;; MAIN ;;
;;;;;;;;;;

(defun %proto-slot (class slot-name strategy &rest keys)
  "Dispatch to the appropriate strategy."
  (case strategy
    ((:simple)
     (apply #'%proto-slot/simple class slot-name keys))
    (:unique-merge
     (apply #'%proto-slot/unique-merge class slot-name keys))
    (t (error "Unknown proto-slot strategy ~S" strategy))))


(defmacro def-proto-slots (class &body (slot-defs))
  "Define prototypal accessor methods on a class' slots.
  Each slot-def is a list like this:
    (slot-name strategy &rest strategy-keyword-args)
  Available strategies and their valid keyword args are:
    :simple
      :base :accessor :reader :writer
    :unique-merge
      :base :accessor :reader :writer :test :key :finder :adder
  See the proto-slots README for details."
  `(progn ,@(mapcar (lambda (slot-def)
                      (apply #'%proto-slot class slot-def))
                    slot-defs)))
