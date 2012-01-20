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

First, you should import the def-proto-slots symbol, e.g. in your
defpackage:

    (defpackage ...
     (:import-from :proto-slots def-proto-slots))

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
              :accessor own-name
              :documentation "Name string. Inherited if nil.")
       (items :initform (list) :initarg :items
              :accessor own-items
              :documentation "List of items. Merged by name.")))

    (def-proto-slots thing
      ((name :simple :base base :reader name)
       (items :unique-merge :base base :reader items
              :key #'name :test #'string= :adder add-item
              :finder find-item :own-finder find-own-item)))


## Inheritance strategies ##

The following inheritance strategies are standard in proto-slots.
Each strategy implements a different style of inheritance:

* :simple

  The slot holds any value. When it is nil, the value is inherited
  from the base object. This strategy accepts the following keyword
  arguments:

  * :base - Func to get the base object. Required!
  * :reader - Func name to get slot value, with inheritance.

* :unique-merge

  The slot holds a list, which is merged with the base object's list
  such that all items are unique (according to the test functions).
  This strategy accepts the following keyword arguments:

  * :base - Func to get the base object. Required!
  * :reader - Func name to get slot value, with inheritance.
  * :test - Func to test whether two items match. Default #'eql.
  * :key - Func to call on items before testing for match.
  * :finder - Func name to find matching item, with inheritance.
  * :own-finder - Func name to find matching item, no inheritance.
  * :adder - Func name to add an item to the list.

* :hash-merge

  The slot holds a hash table, which inherits entries for any keys it
  doesn't already have. This strategy accepts the following keyword
  arguments:

  * :base - Func to get the base object. Required!
  * :reader - Func name to return the hash table, with inheritance.
  * :getter - Func name to look up a key's value, with inheritance.
  * :setter - Func name to set a key's value.

For more details about each strategy, refer to the documentation in
the `docs` directory.

You can also add your own inheritance strategies. Refer to the
`docs/adding-new-strategies.md` file for more information.

You can use the proto-slots:all-strategies function to get a list
of all registered strategies, including non-standard ones.
