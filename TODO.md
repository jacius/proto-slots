
1. Explicit "own" method names, instead of the "own-" prefix.
   This would also eliminate the need for format-symbol.

2. Make inheritance work when the slot is unbound (as opposed to when
   it is set to nil)

3. More inheritance strategies:

   * :concat - simple list (sequence?) concatenation
   * :hash-merge - merge based on hash keys
   * :alist-merge - merge based on alist keys
   * :plist-merge - merge based on plist keys
   * Others...

4. System for registering new  inheritance strategies on-the-fly:

   * (add-proto-strategy symbol func)

     symbol is like :simple. func is a function like
     #'%proto-slot/simple. It takes a class name, slot name, and any
     number of keys.

   * (remove-proto-strategy symbol)
