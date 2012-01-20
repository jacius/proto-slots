# The :hash-merge inheritance strategy #

Hash merge inheritance means that the slot value is a hash table, and
it inherits entries for keys that don't exist in the original hash
table.

Be aware that the value in the hash table does not affect inheritance.
If the key is present, no inheritance occurs, even if the value is
nil. This means you can set the value to nil to effectively disable
inheritance of a certain key.

The following keyword args are valid for this strategy:

* :base

  Name of the function to call on the object to get its base object.
  This arg is required!

* :reader [default: {the value of :accessor}]

  Name for the method to return a new hash table containing all
  entries in the object, including any inherited from the base object
  (which may include items that the base object inherited, and so on).

  If :reader is explicitly nil, no reader method will be defined.

* :getter [default: nil]

  Name for a method to look up the value of a key in the object's hash
  table, possibly one inherited from the base object. This method
  takes two arguments: the key to look up, and the object itself. It
  has multiple return values. The first return value is the value
  found in a hash table, or nil if the key was not found in any hash
  table. The second return value is the object that contained the hash
  table where the key was found, or nil if the key was not found in
  any hash table.

  If :getter is nil (the default), no getter method is defined.

* :setter [default: nil]

  Name for a method to set the value associated with a key in the
  object's hash table. If there was already a value for that key, the
  old value is replaced by the new value. Setting the value of a key
  effectively prevents the object from inheriting that key from its
  base object, even if the value is nil.

  The order of arguments for the method depend on whether it is a setf
  method or not. If the setter name is a list of the form (setf item),
  you use the method like this:

        (setf (item key object) value).

  Otherwise, you use the method like this (in this example, :setter
  was set-item):

        (set-item key object value)

  If :setter is nil (the default), no setter method is defined.

* :remover [default: nil]

  Name for a method to remove a key from the object's hash table.
  Removing a key allows the object to inherit entries with that key
  from its base object.

  The remover method takes two arguments: the key to remove, and the
  object. If the object's hash table does not have the given key, this
  method has no effect.

  If :remover is nil (the default), no remover method is defined.
