# Extending proto-slots #

This guide explains how to extend proto-slots by adding new
inheritance strategies.

New strategies can be added at any time via proto-slots:add-strategy.
It takes two parameters: a strategy name, and a function that
implements the strategy.

* The strategy name identifies the strategy. The name should be a
  symbol, ideally a keyword symbol for greatest convenience. Some
  examples of strategy names are :simple and :unique-merge.

* The function implements the strategy. This can be the symbol name of
  a function, or a function object itself. It is called as part of the
  def-proto-slots macro if the strategy is used by a slot. Strategy
  functions are described more below.

The proto-slots:get-strategy and proto-slots:remove-strategy functions
can be used to query or remove a strategy, respectively. They each
take one parameter: a strategy name.


## Writing Strategy Functions ##

### Requirements ###

A strategy function MUST meet these requirements to work correctly:

* It must take at least two parameters: the class name, and the slot
  name. Additionally, it MAY take one or more key args.

* It must return a list containing a single expression (e.g. a progn
  or let) to define methods to implement the strategy for that slot
  and class. (The function is used within a macro, so it should return
  code as a list to become part of the macro expansion.)

### Guidelines ###

A strategy function SHOULD follow these style guidelines:

* Its name should be similar to the strategy name, so that it is
  recognizable in backtraces, etc.

* It should take a :base keyword arg, which provides the name of a
  function to retrieve the base object. It should immediately signal
  an error if the :base arg is not given, unless the strategy can be
  implemented without retrieving any base object.

* It should have a keyword arg to allow the user to specify the name
  of each method that will be defined. Conversely, it should not
  define any method unless there is a corresponding keyword arg. It
  should not create or use names not provided by the user.

* It should not define a method if the user specifies nil as the name
  for that method. If possible, each method defined by the strategy
  should be independent of the others, so that the user can pick and
  choose which methods should be defined. If a method must depend on
  another method, but the user has specified nil as the name of the
  depended-upon method, an error should be signalled to explain the
  situation.

### Tips ###

Here are some suggestions for writing a good strategy function:

* If appropriate, provide an "own" method (ignoring inheritance) for
  each method that uses inheritance. You usually don't need to define
  a non-inheriting reader, though: the user can just define a standard
  slot reader in their class definition.

* Have your strategy function define generic functions (if needed),
  with useful documentation, before defining the corresponding method.
  This is made simple by the proto-slots:make-method macro. It takes a
  method name, args list, documentation string, and method body. It
  defines a generic function if needed, then defines a method on that
  generic function.
