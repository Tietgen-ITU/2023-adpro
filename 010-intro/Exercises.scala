// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

object MyModule:

  def abs(n: Int): Int =
    if n < 0 then -n else n

  // Exercise 1

  def square(n: Int): Int =
    n * n 

  private def formatAbs(x: Int): String =
    s"The absolute value of ${x} is ${abs(x)}"

  val magic: Int = 42
  var result: Option[Int] = None

  @main def printAbs: Unit =
    assert(magic - 84 == magic.-(84))
    println(formatAbs(magic - 100))

end MyModule

// Exercise 2 requires no programming

// Exercise 3

def fib(n: Int): Int =
  def f(x: Int, acc1: Int, acc2: Int): Int =
    if x < n then
      f(x+1, acc2, acc1+acc2)
    else 
      acc1

  
  f(1, 0, 1)

// Exercise 4

def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = 
  def aux(s: Seq[A]): Boolean =
    s match 
      case Seq(x) => true
      case Seq(x, y, xs*) => ordered(x, y) match
                                  case true => aux(y +: xs)
                                  case false => false
      case Seq() => true

  aux(as.toSeq)

// Exercise 5

def curry[A, B, C](f: (A, B) => C): A => (B => C) =
  (elem: A) => (e2: B) => f(elem, e2)

def isSortedCurried[A]: Array[A] => ((A, A) => Boolean) => Boolean =
  (as: Array[A]) => (f: (A, A) => Boolean) => isSorted(as, f)

// Exercise 6

def uncurry[A, B, C](f: A => B => C): (A, B) => C =
  (a:A, b:B) => f(a)(b)

def isSortedCurriedUncurried[A]: (Array[A], (A, A) => Boolean) => Boolean =
  (as: Array[A], or: (A,A) => Boolean) => isSorted(as, or)

// Exercise 7

def compose[A, B, C](f: B => C, g: A => B): A => C =
  g andThen f
