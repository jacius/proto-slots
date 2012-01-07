
* Make inheritance work when the slot is unbound (as opposed to when
  it is set to nil)

* More inheritance strategies:

  * :concat - simple list (sequence?) concatenation
  * :hash-merge - merge based on hash keys
  * :alist-merge - merge based on alist keys
  * :plist-merge - merge based on plist keys
  * Others...

* System for registering new  inheritance strategies on-the-fly:

  * (add-proto-strategy symbol func)

    symbol is like :simple. func is a function like
    #'%proto-slot/simple. It takes a class name, slot name, and any
    number of keys.

  * (remove-proto-strategy symbol)
