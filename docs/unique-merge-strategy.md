# The :unique-merge inheritance strategy #

Unique merge inheritance means that the slot value is a list, and it
inherits from the base object's list every items that does not match
any item in the object's own list. See the description for :reader
below for details about how a match is checked.

The following keyword args are valid for this strategy:

* :base

  Name of the function to call on the object to get its base object.
  This arg is required!

* :reader [default: {the value of :accessor}]

  Name for the method to read the value of the slot, possibly
  including items inherited from the base object (which may include
  items the base object inherited from its base, and so on).

  The object does not inherit any item from the base object's list if
  that item matches an item in the object's own list. The functions
  provided for the :test and :key args are used to check whether
  items match, a la the standard #'union function.

  If :reader is explicitly nil, no reader method will be defined.

* :test [default: #'eql]

  An existing function used (together with :key) to test for matches.
  It has the same semantics as the :test keyword arg to the standard
  functions #'find, #'union, etc.

  In :reader, it is used to compare two items to check whether an
  inherited item matches any item in the object's own list.
  In :finder, it is used to check whether any item matches the query.
  In :adder, it is used to check whether the new item should replace
  an item currently in the object's own list.

* :key [default: #'identity]

  An existing function to apply to each item before calling the :test
  function to check for a match. It has the same semantics as the :key
  keyword arg to the standard functions #'find, #'union, etc.

* :finder [default: nil]

  Name for a method to find the first matching item in the object's
  list, possibly one inherited from the base object. This method
  returns nil if no matching item is found. If :finder is nil (the
  default), no finder method is defined.

  The finder method takes two arguments: the object itself, and a
  query. The query is used by the standard #'find function, using the
  :key and :test strategy keyword args. Note that the :key function
  is not called on the query, only on the items being tested. In
  other words, the query itself must match the result of calling the
  :key function on an item.

* :own-finder [default: nil]

  Name for a method to find the first matching item in the object's
  own list, ignoring inheritance. This method returns nil if no
  matching item is found. If :own-finder is nil (the default), no
  own-finder method is defined.

* :adder [default: nil]

  Name for a method to add a new item to the object's own list. If
  :adder is nil (the default), no adder method is defined.

  If any item in the object's own list matches the new item, that
  item is removed before adding the new item. The functions provided
  for the :test and :key args are used to check whether items match,
  a la the standard #'union function.
