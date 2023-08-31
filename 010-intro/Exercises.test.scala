// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.intro

import java.util.concurrent.TimeoutException

import scala.concurrent.Await
import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionException
import scala.concurrent.Future
import scala.concurrent.duration.*

import scala.util.{Try, Failure}

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalacheck.Prop.*

def fasterThan[A](d: Duration) (b: => A): Boolean =
  import ExecutionContext.Implicits.global
  Try { Await.result(Future(b), d) }.isSuccess

// Exercise 1
object Exercise01Spec
  extends org.scalacheck.Properties("intro"):

  property("Ex01.00: square behaves like n * n") = 
    forAll { (n: Int) => MyModule.square(n) == n * n }

// Exercise 3
object Exercise03Spec
  extends org.scalacheck.Properties("intro"):
 
  property("Ex03.01: F1 Fibonacci number is 0") = fib(1) == 0
  property("Ex03.02: F2 Fibonacci number is 1") = fib(2) == 1
  property("Ex03.03: F3 Fibonacci number is 1") = fib(3) == 1
  property("Ex03.04: F4 Fibonacci number is 2") = fib(4) == 2

  property("Ex03.05: Fibonacci numbers are positive for positive args") = 
    forAll(Gen.choose(1, 30)) { (n: Int) => fib(n) >= 0 }

  property("Ex03.06: Each Fibonacci number is a sum of the two predecessors") =
    forAll(Gen.choose(3, 30)) { (n: Int) =>
      fib(n) == fib(n - 1) + fib(n - 2) }


// Shared
val comp: (Int, Int) => Boolean     = _ <= _
val invComp: (Int, Int) => Boolean  = _ >= _
val always: (Int, Int) => Boolean = (_, _) => true 
val never: (Int, Int) => Boolean = (_, _) => false

val intArray: Gen[Array[Int]] = 
  Gen.containerOf[Array,Int](Gen.choose(0, 2000))

val nonTrivialIntArray: Gen[Array[Int]] = 
  intArray filter { ints => ints.size > 1 }

// Exercise 4
object Exercise04Spec
  extends org.scalacheck.Properties("intro"):

  property("Ex04.00: Array(1 to 6)") = 
    isSorted(Array(1 to 6*), comp)

  property("Ex04.01: Array(6,2,3,4,5,6)") = 
    !isSorted(Array(6, 2, 3, 4, 5, 6), comp)

  property("Ex04.02: Array(1,2,3,4,5,1)") =
    !isSorted (Array(1, 2, 3, 4, 5, 1), comp)

  property("Ex04.03: An array after standard sorting is sorted") =
    forAll(intArray) { ints => isSorted[Int](ints.sorted, comp) }

  property("Ex04.04: An array is sorted if all values equal in the ordering") =
    forAll(intArray) { ints => isSorted[Int](ints, always) }

  property("Ex04.05: No array is sorted if no values equal in an ordering") =
    forAll(nonTrivialIntArray) { ints => !isSorted[Int](ints, never) }

  property("Ex04.06: Array(1 to 6*) is not sorted if inversed") =
    !isSorted (Array(1 to 6*).reverse, comp)

  property("Ex04.07: Array(1 to 6*) is not sorted if ordering inversed") =
    !isSorted (Array(1 to 6*), invComp)

  property("Ex04.08: Array(6,2,3,4,5,6) is not sorted in inverse order") =
    !isSorted (Array(6,2,3,4,5,6), invComp)

  property("Ex04.09: Array(1,1,1,1,1,1) is sorted in inverse order") =
    isSorted (Array(1,1,1,1,1,1), invComp)

  property("Ex04.10: Array(6,6,4,4,2,1) is sorted in inverse order") = 
    isSorted (Array(6,5,4,4,2,1), invComp)

  property("Ex04.11: An empty array is sorted in any ordering") = 
    forAll { (order: (Int,Int) => Boolean) => 
      isSorted (Array[Int](), order) }

  property("Ex04.12: A singleton array is sorted in any ordering") = 
    forAll { (order: (Int,Int) => Boolean) => 
      forAll { (n: Int) => isSorted (Array[Int](n), order) } }

  property("Ex04.13: fib(n) should be linear-time in n (v. fast)") = 
    fasterThan(50.millis) { fib(50) }
    // scalacheck has within: 
    // within(50) { fib(50); true }
    // but this waits for the property to complete before failing which 
    // is not practical for us. Our fasterThan kills after 50.millis
    
// Exercise 5
object Exercise05Spec
  extends org.scalacheck.Properties("intro"):

  property("Ex05.00: Currying doesn't change value of isSorted [sorted]") =
    forAll(intArray) { ints =>
      isSorted(ints.sorted, comp) == curry (isSorted[Int]) (ints.sorted) (comp) }

  property("Ex05.01: Currying doesn't change value of isSorted [arbitrary]") =
    forAll { (order: (Int, Int) => Boolean) =>
      forAll(intArray) { ints =>
        isSorted(ints, order) == curry (isSorted[Int]) (ints) (order) } }

  property("Ex05.02: Currying doesn't change value of a function") =
    forAll { (f: (Int,Int) => Int) =>
      forAll { (n: Int, m: Int) =>
        f(n, m) == curry(f) (n) (m) } }

  property("Ex05.03: isSortedCurried is equivalent to isSorted") =
    forAll { (order: (Int, Int) => Boolean) =>
      forAll(intArray) { ints => 
        isSorted(ints, order) == isSortedCurried(ints) (order) } }

  // Exercise 6
object Exercise06Spec
  extends org.scalacheck.Properties("intro"):

  property("Ex06.00: Uncurrying doesn't change value of isSortedCurried [sorted]") =
    forAll(intArray) { ints =>
      isSortedCurried(ints.sorted) (comp) == 
        uncurry (isSortedCurried[Int]) (ints.sorted, comp) }

  property("Ex06.01: Uncurrying doesn't change value of isSortedCurried [arbitrary]") =
    forAll { (order: (Int, Int) => Boolean) =>
      forAll(intArray) { ints =>
        isSortedCurried(ints) (order) == 
          uncurry (isSortedCurried[Int]) (ints, order) } }

  property("Ex06.02: Uncurrying doesn't change value of a function") =
    forAll { (f: Int => Int => Int) =>
      forAll { (n: Int, m: Int) =>
        f(n) (m) == uncurry(f) (n, m) } }

  property("Ex06.03: isSortedCurriedUncurried is equivalent to isSortedCurried") =
    forAll { (order: (Int, Int) => Boolean) =>
      forAll(intArray) { ints => 
        isSortedCurriedUncurried(ints, order) == isSortedCurried(ints) (order) } }

  property("Ex06.04: isSortedCurriedUncurried is equal to isSorted") =
    forAll { (order: (Int, Int) => Boolean) =>
      forAll(intArray) { ints => 
        isSortedCurriedUncurried(ints, order) == isSorted(ints, order) } }

  // Exercise 7
object Exercise07Spec
  extends org.scalacheck.Properties("intro"):

  property("Ex07.00: Associative (compose(compose (f,g),h) == compose (f, compose(g,h))") = 
    forAll { (f :Int => Int, g: Int => Int, h: Int => Int) =>
      forAll { (n: Int)  =>
        compose(compose(f, g), h) (n) == 
          compose(f, compose (g, h)) (n) } }

  
  property("Ex07.01: Identity is a zero for left composition: compose (identity, f) == f") =
    forAll { (f: String => String) =>
      forAll { (s: String) =>
        compose (identity[String], f) (s) == f (s) } }

  property("Ex07.02: Idenitty is a zero for right composition: compose (f, identity) == f") = 
    forAll { (f: Int => String) =>
      forAll  { (n: Int) =>
        compose (f, identity[Int]) (n) == f (n) } }
