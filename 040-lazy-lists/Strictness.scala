def id[A](a : A) : A =
  println(a)
  a

def if1[A](cond : Boolean, onTrue : A, onFalse : A) : A =
  if cond then onTrue else onFalse

def if2[A](cond : Boolean, onTrue : () => A, onFalse : () => A) : A =
  if cond then onTrue() else onFalse()

def if3[A](cond : Boolean, onTrue : => A, onFalse : => A) : A =
  if cond then onTrue else onFalse

def testIf() =
  if true then id(23) else id(42)       // Prints 23          -- if is a non-strict language construct.
  if1(true, id(23), id(42))             // Prints 23 *and* 42 -- if1 is strict!
  if2(true, () => id(23), () => id(42)) // Prints 23          -- if2 simulates non-strictness.
  if3(true, id(23), id(42))             // Prints 23          -- if3 is non-strict.

// We define a "when" construct. It is impure, but it illustrates the
// idea of using thunks "extend" the core language in a light-weight
// way. Function "when" executes its body if the condition evaluates
// to true and otherwise does nothing.
def whenByValue(cond : Boolean)(body : Unit) : Unit = if cond then body
def whenByName(cond : Boolean)(body : => Unit) : Unit = if cond then body

def testWhen() =
  // Call-by-value semantics means: re-write expression to replace
  // "body" with the result of evaluating "e".
  whenByValue(false){
    println("By value") // (whenByValue(p)(e) && e ~> v) ~> if p then v
  }
  // Call-by-name semantics means: re-write expression to replace
  // "body" with "e" itself.
  whenByName(false){
    println("By name") // whenByName(p)(e) ~> if p then e
  }
