# The :simple inheritance strategy #

Simple inheritance means that when the slot is nil or unbound, it
inherits the value from the base object (if there is one). The base
object might inherit from its own base, so the chain of inheritance
will be followed until a non-nil value is found, or until an object in
the chain has no base.

The following keyword args are valid for this strategy:

* :base

  Name of the function to call on the object to get its base object.
  This arg is required!

* :reader [default: {the value of :accessor}]

  Name for the method to read the value of the slot. If the slot value
  is nil or unbound, and the object has a base object, this method
  returns the base object's value of this slot (which may be inherited
  from the base object's own base object, and so on down the
  inheritance chain).

  If :reader is explicitly nil, no reader method will be defined.
