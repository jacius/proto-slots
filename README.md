# proto-slots #

Version:   1.0  
Released:  2012-01-04  
Homepage:  https://github.com/jacius/proto-slots  
Author:    John Croisant <john@croisant.net>  
Copyright: © 2012 John Croisant  
License:   X11/MIT (see LICENSE.txt)  
Summary:   Prototypal inheritance for the Common Lisp Object System  

proto-slots provides a macro for defining prototypal accessor methods
so that CLOS instances will support protoypal inheritance. Prototypal
inheritance means that an instance's slots can inherit values from
another instance, known in these docs as the "base object" (but more
commonly known elsewhere as the "prototype").


## Usage ##

First, you should either use the proto-slots package or import the
def-proto-slots symbol, e.g. in your defpackage:

    (defpackage ...
     (:use :proto-slots))

Later, after you have defined your class using defclass, use the
def-proto-slots macro to define the prototypal accessors:
 
    (def-proto-slots class-name
      (slot-defs...))

Each slot-def is a list of this form:

    (slot-name strategy &rest strategy-keyword-args)

strategy is a keyword symbol that indicates the inheritance strategy
to use for that slot (see below). strategy-keyword-args are keyword
args used by the chosen strategy. Each strategy has its own keywords
with their own semantics.

Here's an example of defining a class, then calling def-proto-slots to
define prototypal accessors for two of its slots:

    (defclass thing ()
      ((base  :initform nil :initarg :base
              :accessor base
              :documentation "Base object for inheritance.")
       (id    :initform (gensym)
              :reader id
              :documentation "Per-instance unique ID. Not inherited.")
       (name  :initform nil :initarg :name
              :documentation "Name string. Inherited if nil.")
       (items :initform (list) :initarg :items
              :documentation "List of items. Merged by name.")))

    (def-proto-slots thing
      ((name :simple :base base)
       (items :unique-merge :base base
              :adder add-item :finder find-item
              :key #'name :test #'string=)))


## Inheritance strategies ##

The following inheritance strategies are currently available.
Each strategy implements a different style of inheritance:

* :simple

  The slot holds any value. When it is nil, the value is inherited
  from the base object.

* :unique-merge

  The slot holds a list, which is merged with the base object's list
  such that all items are unique (according to the test functions).

Each strategy is described in more detail below.


### :simple ###

Simple inheritance means that when the slot is null, it inherits the
value from the base object (if there is one). The base object might
inherit from its own base, so the chain of inheritance will be
followed until a non-nil value is found, or until an object in the
chain has no base.

The simple inheritance strategy may (depending on the keyword args)
define the following methods:

* {the value of :reader}
* {the value of :own-reader}
* {the value of :writer}

The following keyword args are valid for this strategy:

* :base

  Name of the function to call on the object to get its base object.
  This arg is required!

* :accessor [default: {the slot-name}]

  Name for reader and writer methods. See :reader and :writer below.
  This defaults to the slot name, so you only have to give :accessor,
  :reader, or :writer if you want them to have a different name than
  the slot.

* :reader [default: {the value of :accessor}]

  Name for the method to read the value of the slot. If the slot
  value is nil, and the object has a base object, this method returns
  the base object's value of this slot (which may be inherited from
  the base object's own base object, and so on down the inheritance
  chain).

  The :reader arg takes precedence over the :accessor arg for the
  purpose of naming the reader method. If :reader is explicitly nil,
  no reader method will be defined.

* :own-reader [default: nil]

  Name for the method to read the direct value of the slot, without
  checking inheritance. This method always returns the value of the
  slot, even if the value is nil. If :own-reader is nil (the default),
  no own-reader method will be defined.

* :writer [default: (setf {the value of :accessor})]

  Name for the method to replace (i.e. setf) the value of the slot.
  This affects only the object it's called on, it never affects the
  base object.

  The :writer arg takes precedence over the :accessor arg for the
  purpose of naming the writer methods. If :writer is explicitly nil,
  no writer method will be defined.


### :unique-merge ###

Unique merge inheritance means that the slot value is a list, and it
inherits from the base object's list every items that does not match
any item in the object's own list. See the description for :reader
below for details about how a match is checked.

The union inheritance strategy may (depending on the keyword args)
define the following methods:

* {the value of :reader}
* {the value of :own-reader}
* {the value of :writer}
* {the value of :finder}
* {the value of :own-finder}
* {the value of :adder}

The following keyword args are valid for this strategy:

* :base

  Name of the function to call on the object to get its base object.
  This arg is required!

* :accessor [default: {the slot-name}]

  Name for reader and writer methods. See :reader and :writer below.
  This defaults to the slot name, so you only have to give :accessor,
  :reader, or :writer if you want them to have a different name than
  the slot.

* :reader [default: {the value of :accessor}]

  Name for the method to read the value of the slot, possibly
  including items inherited from the base object (which may include
  items the base object inherited from its base, and so on).

  The object does not inherit any item from the base object's list if
  that item matches an item in the object's own list. The functions
  provided for the :test and :key args are used to check whether
  items match, a la the standard #'union function.

  The :reader arg takes precedence over the :accessor arg for the
  purpose of naming the reader method. If :reader is explicitly nil,
  no reader method will be defined.

* :own-reader [default: nil]

  Name for the method to read the direct value of the slot, without
  checking inheritance. If :own-reader is nil (the default), no
  own-reader method will be defined.

* :writer [default: (setf {the value of :accessor})]

  Name for the method to write (i.e. setf) the value of the slot. The
  new value must be a list, or things will break later on. This
  method affects only the object it's called on, it never affects the
  base object.

  The :writer arg takes precedence over the :accessor arg for the
  purpose of naming the writer method. If :writer is explicitly nil,
  no writer method will be defined.

* :test [default: #'eql]

  A function used (together with :key) to test for matches. It has
  the same semantics as the :test keyword arg to the standard
  functions #'find, #'union, etc.

  In :reader, it is used to compare two items to check whether an
  inherited item matches any item in the object's own list.
  In :finder, it is used to check whether any item matches the query.
  In :adder, it is used to check whether the new item should replace
  an item currently in the object's own list.

* :key [default: #'identity]

  A function to apply to each item before calling the :test function
  to check for a match. It has the same semantics as the :key keyword
  arg to the standard functions #'find, #'union, etc.

* :finder [default: nil]

  Name for a method to find a matching item in the object's list,
  possibly one inherited from the base object. If :finder is nil (the
  default), no finder method is defined.

  The finder method takes two arguments: the object itself, and a
  query. The query is used by the standard #'find function, using the
  :key and :test strategy keyword args. Note that the :key function
  is not called on the query, only on the items being tested. In
  other words, the query itself must match the result of calling the
  :key function on an item.

* :own-finder [default: nil]

  Name for a method to find a matching item in the object's own list,
  ignoring inheritance. This method returns nil if no matching item is
  found. If :own-finder is nil (the default), no own-finder method is
  defined.

* :adder [default: nil]

  Name for a method to add a new item to the object's own list. If
  :adder is nil (the default), no finder method is defined.

  If any item in the object's own list matches the new item, that
  item is removed before adding the new item. The functions provided
  for the :test and :key args are used to check whether items match,
  a la the standard #'union function.
