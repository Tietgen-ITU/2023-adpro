// Like "Nothing", but it does _not_ hold that forall t . EmptyType <: t.
// That is reserved only for "Nothing".

enum EmptyType:
  case E(e: EmptyType)

enum Op:
  case Add
  case Sub
  case Mul
  case Div

// An ADT modeling an abstract syntax tree. Note the seemingly useless
// co-variant type parameter A
enum AST[+A]:
  case ConstInt(c : Int) extends AST[Nothing]
  case BinOp[A](a : A, op : Op, lhs : AST[A], rhs : AST[A]) extends AST[A]

  // There are no guarantees here that we are dealing with a ConstInt
  // value. We choose to throw an exception in non-ConstInt cases.
  def toInt() = this match
    case ConstInt(x) => x
    case _ => throw Exception("Not a constant")

// This type is inhabited by both, ConstInt and BinOp:
// - ConstInt only needs an integer argument, it does not care about the type of A.
// - BinOp needs an instance of A - in this case the only value inhabiting Unit, ().
type Expr = AST[Unit]

// Short-hand for constructing binary expressions.
def binop(op : Op, lhs : Expr, rhs : Expr) : Expr =
  AST.BinOp((), op, lhs, rhs)

// This type is not inhabited by BinOp, because that would require to
// call the BinOp constructor with a value of type EmptyType, which we
// cannot construct.
// Note: no sub-typing relation between ValueA and Expr
type ValueA = AST[EmptyType]

// Short-hand to construct a constant expression. Try to change the
// return type from Expr to ValueA and recompile.
def constA(x : Int) : Expr = AST.ConstInt(x)

// This type is not inhabited by BinOp, because that would require to
// call the BinOp constructor with a value of type Nothing, which we
// cannot construct.
// Note: ValueB <: Expr because Nothing <: Unit
type ValueB = AST[Nothing]

// Short-hand to construct a constant expression.
def constB(x : Int) : ValueB = AST.ConstInt(x)

// The following two functions are now guaranteed to never fail, but
// unfortunately, the Scala compiler does not see that.

def toIntA(c : ValueA) : Int = c.toInt()
  // Using explicit pattern matching:
  // c match
  //   case AST.ConstInt(x) => x // Warns with "pattern matching may not be exhaustive"

def toIntB(c : ValueB) : Int = c.toInt()
  // Using explicit pattern matching:
  // c match
  //   case AST.ConstInt(x) => x // Warns with "pattern matching may not be exhaustive"

// If you are curious, try to implement an evaluation function for
// each variant. There is a subtle difference between evalA and evalB
// owned to co-variance.
def evalA(e : Expr) : ValueA = ???

def evalB(e : Expr) : ValueB = ???

val e1 = binop(Op.Add, constA(1), constA(2))
val e2 = binop(Op.Add, constB(1), constB(2))
// val _ = toIntA(constA(1))     // Does not typecheck.
val _ = toIntA(evalA(constA(1))) // Typechecks.
val _ = toIntB(constB(1))        // Typechecks.
val _ = toIntB(evalB(constB(1))) // Typechecks.
// val _ = toIntA(e1)            // Does not typecheck.
// val _ = toIntB(e2)            // Does not typecheck.
