// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro
package adt


import java.util.NoSuchElementException
import scala.util.{Try, Failure}

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import List.*



// A test data generation setup is needed to run the tests on a
// new data type (List).  We will explain later in the course
// what it does.  You can skip to the tests for exercises

def listOf[A](using Arbitrary[A]): Gen[List[A]] =
  for scalaList <- Gen.listOf[A](arbitrary[A])
  yield List(scalaList*)

def listOfN[A](n: Int) (using Arbitrary[A]): Gen[List[A]] =
  for scalaList <- Gen.listOfN[A](n, arbitrary[A])
  yield List(scalaList*)

def nonEmptyListOf[A](using Arbitrary[A]): Gen[List[A]] =
  listOf[A] filter { _ != Nil }

given [A] (using Arbitrary[A]): Arbitrary[List[A]] =
  Arbitrary (listOf[A])

// have to use pattern matching and recursion, as HOFs are
// initially not implemented, and we want this to work robustly
def toScala[A] (l: List[A]): scala.collection.immutable.List[A] =
  l match
    case Nil => scala.collection.immutable.Nil
    case Cons(h, t) => h :: toScala(t)


// Reusable tests for length (use the same tests for length & length1)
def lenSpec[A: Arbitrary](fn: String, f: List[A] => Int) =
  Seq[(String, Prop)] (
    (f"00: $fn of an empty list is 0",
      f(Nil) == 0 ),

    (f"01: $fn of a singleton is 1",
      forAll { (a: A) => f(List(a)) ?= 1 } ),

    (f"02: $fn(l) grows by one when we Cons",
      forAll { (l: List[A], a: A) => f(l) + 1 == f(Cons(a, l)) }),
  )

  // Exercise 1 requires no programming



  // Exercise 2 (tail)
object ExerciseSpec02
  extends org.scalacheck.Properties("intro"):

  property("Ex02.00: tail on an empty list throws NoSuchElement") =
    Try { tail(Nil) } match
      case Failure(e: NoSuchElementException) => true
      case _ => false

  property("Ex02.01: tail is an inverse of Cons") =
    forAll { (x: Int, l: List[Int]) => tail(Cons(x, l)) == l }

  // `eq` is reference equality in Scala
  property("Ex02.02: tail should not copy the list") =
    forAll { (x: Int, l: List[Int]) => tail(Cons(x, l)) eq l }



  // Exercise 3 (drop)
object ExerciseSpec03
  extends org.scalacheck.Properties("intro"):

  property("Ex03.00: drop(l, 1) should behave like tail(l) [nonempty]") =
     forAllNoShrink(nonEmptyListOf[Int]) { l => tail(l) eq drop(l, 1) }

  property("Ex03.01: drop(Nil, 0) is Nil") =
    drop(Nil, 0) == Nil

  property("Ex03.02: drop(Nil, n) throws NoSuchElement [n > 0]") =
    forAllNoShrink(arbitrary[Int].filter { _ > 0 }) { (n: Int) =>
      Try { drop(Nil, n) } match
        case Failure(e: NoSuchElementException) => true
        case _ => false
    }

  property("Ex03.03: tail(drop(l,n)) == drop(tail(l),n) == drop(l, n+1)") =
    forAllNoShrink(Gen.choose(1, 100)) { n =>
      forAllNoShrink(listOfN[Int] (n * 2)) { l =>
        drop (l, n + 1).eq(tail(drop(l, n)))
          && drop (l, n + 1).eq(drop(tail(l), n))
    } }

  property("Ex03.04: drop(l, n) throws NoSuchElement if l shorter than n") =
    forAllNoShrink(Gen.choose(0, 100)) { n =>
      forAllNoShrink(listOfN[Int] (n)) { l =>
        Try { drop (l, n+1) } match
          case Failure(e: NoSuchElementException) => true
          case _ => false
  } }

  property("Ex03.05: drop(l, n) is identity for any non-positive n") =
    forAllNoShrink(arbitrary[Int].filter { _ <= 0}) { n =>
      forAll { (l: List[Int]) => drop(l, n) eq l
    } }



  // Exercise 4 (dropWhile)
object ExerciseSpec04
  extends org.scalacheck.Properties("intro"):

  property("Ex04.00: dropWhile(Nil, p) is Nil for any p") =
    forAll { (p: Int => Boolean) => dropWhile(Nil, p) ?= Nil }

  property("Ex04.01: dropWhile with a true predicate empties the list") =
    forAll { (l: List[Int]) => dropWhile(l, _ => true) ?= Nil }

  property("Ex04.02: dropWhile with a false predicate is identity") =
    forAll { (l: List[Int]) => dropWhile(l, _ => false) ?= l }

  property("Ex04.02: dropWhile with a false does not copy the list") =
    forAll { (l: List[Int]) => dropWhile(l, _ => false) eq l }

  property("Ex04.03: head(dropWhile(l, p)) violates p or is empty") =
    forAll  { (l: List[Int], p: Int => Boolean) =>
      dropWhile(l, p) match // patternMatching as headOption not yet implemented
        case Cons(h, _) => !p (h)
        case _ => true
    }

  property("Ex04.04: If head satisfies p then dropWhile from tail identical") =
    forAll { (p: Int => Boolean) =>
      forAllNoShrink(nonEmptyListOf[Int]) { l =>
        if p(head(l))
          then dropWhile(l, p) eq (dropWhile(tail(l), p))
          else dropWhile(l, p) eq l
    } }



  // Exercise 5 (init)
object ExerciseSpec05
  extends org.scalacheck.Properties("intro"):

  property("Ex05.00: init(Nil) throws NoSuchElement") =
    Try { init(Nil) } match
      case Failure(e: NoSuchElementException) => true
      case _ => false

  property("Ex05.01: forall l, a: init (l ++ List(a)) == l") =
    forAll(listOf[Int], arbitrary[Int]) { (l, a) =>
      init (append(l, List(a))) ?= l
    }

  property("Ex05.02: for 2+ elems init(Cons(a,l)) == Cons(a, init(l))") =
    forAllNoShrink(nonEmptyListOf[Int]) { l =>
      init(Cons(42, l)) ?= Cons(42, init(l)) }



  // Exercise 6 (length)

  // Reusable tests for length (use the same tests for length & length1)

  def lenSpec[A: Arbitrary](fn: String, f: List[A] => Int) =
    Seq[(String, Prop)] (
      (f"00: $fn of an empty list is 0",
        f(Nil) == 0 ),

      (f"01: $fn of a singleton is 1",
        forAll { (a: A) => f(List(a)) ?= 1 } ),

      (f"02: $fn(l) grows by one when we Cons",
        forAll { (l: List[A], a: A) => f(l) + 1 == f(Cons(a, l)) }),
    )
object ExerciseSpec06
  extends org.scalacheck.Properties("intro"):

  // Tried using `include` but tests were run twice (perhaps
  // some scala-cli mess), so resorted to manual (imperative) management
  for (n, p) <-
    Try(lenSpec[Char]("length", length))
      .getOrElse(Seq(0 -> Prop.falsified,
                     1 -> Prop.falsified,
                     2 -> Prop.falsified))
  do property(s"Ex06.$n") = p



  // Exercise 7 (foldLeft)
object ExerciseSpec07
  extends org.scalacheck.Properties("intro"):

  property("Ex07.00: foldLeft returns the initial value for Nil") =
    forAll { (z: Int, f: (Int, String) => Int) =>
      foldLeft(Nil, z, f) == z }

  property("Ex07.01: (((0-1)-2)-3)-4 = -10, using foldLeft") =
    foldLeft(List(1, 2, 3, 4), 0, _ - _) ?= -10

  property("Ex07.02: foldLeft computes summation ok") =
    forAll { (l: collection.immutable.List[Int]) =>
      foldLeft(List(l*), 0, _ + _) == l.sum }

  property("Ex07.03: foldLeft returns z for operator returning z") =
    forAll { (l: List[Int], z: Char) =>
      foldLeft(l, z, (z,a) => z) == z }

  property("Ex07.04: foldLeft returns last for operator returning a") =
    forAllNoShrink(Gen.nonEmptyListOf[Int](arbitrary[Int])) { l =>
      l.last == foldLeft(List(l*), 42, (z,a) => a)  }

  property("Ex07.05: foldLeft returns const for const operator [non-empty]") =
    forAllNoShrink(nonEmptyListOf[Int]) { l =>
      forAll { (z: Int, c: Int) =>
        foldLeft(l, z, (z,a) => c) == c
    } }

  property("Ex07.06: foldLeft compiles with @annotation.tailrec") =
    val annotations = adpro.annotations[List$]("foldLeft")
    annotations.exists { _ == "scala.annotation.tailrec" }



  // Exercise 8 (product and length1)
object ExerciseSpec08
  extends org.scalacheck.Properties("intro"):

  // Tried using `include` but tests were run twice (perhaps
  // some scala-cli mess), so resorted to manual (imperative) management
  for (n, p) <-
    Try(lenSpec[Char]("length1", length1))
      .getOrElse(Seq(0 -> Prop.falsified,
                     1 -> Prop.falsified,
                     2 -> Prop.falsified))
  do property(s"Ex08.$n") = p

  property("Ex08.03: product of empty list is 1") = product(Nil) ?= 1

  property("Ex08.04: product of a singleton is equal to its only value") =
    forAll { (n: Int) => product(List(n)) ?= n }

  property("Ex08.05: product behaves like in the standard library") =
    forAll { (l: scala.collection.immutable.List[Int]) =>
        product (List(l*)) ?= l.product }



  // "Exercise 9 (reverse)
object ExerciseSpec09
  extends org.scalacheck.Properties("intro"):

  property("Ex09.00: reverse of a Nil is Nil") = reverse(Nil) ?= Nil

  property("Ex09.01: reverse of a singleton is the same list") =
    forAll { (n: Int) => reverse(List(n)) ?= List(n) }

  property("Ex09.02: reverse is its own inverse") =
    forAll { (l: List[Int]) => reverse(reverse(l)) ?= l }

  property("Ex09.03: reverse(List(1, 2, 3, 4, 42)) is List(42, 4, 3, 2, 1)") =
    reverse(List(1, 2, 3, 4, 42)) ?= List(42, 4, 3, 2, 1)

  property("Ex09.04: reverse of an asymetric list is not identity") =
    forAll { (l: List[Int]) =>
      val l1 = Cons(43, reverse(Cons(42, l)))
      reverse(l1) != l1
    }



  // Exercise 10 (foldRight1)
object ExerciseSpec10
  extends org.scalacheck.Properties("intro"):

  property("Ex10.00: A sequence of subtractions same as with foldRight") =
    forAll { (l: List[Int]) =>
      forAll (Gen.choose(-5000,5000)) { z =>
        foldRight1(l, z, _ - _) == foldRight(l, z, _ - _) } }

  property("Ex10.01: the order of evaluations is the same as for foldRight") =
    forAll { (l: List[Int]) =>
      foldRight1[Int,List[Int]](l, Nil, Cons(_, _)) ==
        foldRight[Int, List[Int]](l, Nil, Cons(_, _)) }

  property("Ex10.02: foldRight1 is equivalent to foldRight") =
    forAll { (l: List[Int], z: Int, f: (Int, Int) => Int) =>
      foldRight1(l, z, f) == foldRight(l, z, f) }

  // Exercise 11 (foldLeft1)
object ExerciseSpec11
  extends org.scalacheck.Properties("intro"):

  property("Ex11.00: check a sequence of subtractions against foldLeft") =
    forAll { (l: List[Int]) =>
      forAll (Gen.choose(-5000,5000)) { z =>
        foldLeft1(l, z, _ - _) == foldLeft(l, z, _ - _) } }

  property("Ex11.01: the order of evaluations is the same as for foldLeft") =
    forAll { (l: List[Int]) =>
      foldLeft1[Int,List[Int]](l, Nil, (z, x) => Cons(x, z)) ==
        foldLeft[Int, List[Int]](l, Nil, (z, x) => Cons(x, z)) }

  property("Ex11.02: foldLeft1 is equivalent to foldLeft") =
    forAll { (l: List[Int], z: Int, f: (Int, Int) => Int) =>
      foldLeft1(l, z, f) == foldLeft(l, z, f) }



  // Exercise 12 (concat)
object ExerciseSpec12
  extends org.scalacheck.Properties("intro"):

  property("Ex12.00: Append of Nil is Nil") = { concat(Nil) ?= Nil }

  property("Ex12.01: Append of empty lists is an empty list") =
    forAll { (l: List[Unit]) => concat(map(l, _ => Nil)) ?= Nil }

  property("Ex12.02: Append of singletons maintains the same length") =
    forAll { (l: List[Int]) =>
      length(concat(map(l, List(_)))) ?= length(l) }

  property("Ex12.03: Check behavior against the standard library of Scala") =
    forAll { (l: List[List[Int]]) =>
      concat(l) ?= List(toScala(l).map(toScala[Int]).flatten*) }



  // Exercise 13 (filter)
object ExerciseSpec13
  extends org.scalacheck.Properties("intro"):

  property("Ex13.00: Filter on Nil is Nil") =
    forAll { (p: Int => Boolean) => filter[Int](Nil, p) ?= Nil }

  property("Ex13.01: Filter with false predicate is Nil") =
    forAll { (l: List[Int]) => filter(l, _ => false) ?= Nil }

  property("Ex13.02: Filter with true predicate is identity") =
    forAll { (l: List[Int]) => filter(l, _ => true) ?= l }

  property("Ex13.03: Filter does not loose elements") =
    forAll { (l: List[Int], p: Int => Boolean) =>
      length(filter(l, p)) + length(filter(l, !p(_))) ?= length(l) }

  property("Ex13.04: Check behavior against the standard library of Scala") =
    forAll { (l: List[Int], p: Int => Boolean) =>
      filter(l, p) ?= List(toScala(l).filter(p)*) }

  // Exercise 14 (flatMap)
object ExerciseSpec14
  extends org.scalacheck.Properties("intro"):

  property("Ex14.00: flatMap(List(1,2,3)) (i => List(i,i)) is List(1,1,2,2,3,3)") =
    flatMap(List(1, 2, 3), i => List(i,i)) ?= List(1, 1, 2, 2, 3, 3)

  property("Ex14.01: flatMap with identity is concat") =
    forAll { (l: List[List[Int]]) => flatMap (l, identity) ?= concat(l) }

  property("Ex14.02: flatMap with unit is map") =
    forAll { (l: List[Int], f: Int => Int) =>
      flatMap(l, x => List(f(x))) ?= map(l, f) }

  property("Ex14.03: monad associativity law for flatMap") =
    given Arbitrary[List[Int]] = Arbitrary {
      for
        n <- Gen.choose(0, 30)
        l <- listOfN[Int](n)
      yield l }

    forAll { (l: List[Int], f: Int => List[Int], g: Int => List[Int]) =>
      flatMap(flatMap(l, f),g) ?= flatMap(l, x => flatMap(f(x),g)) }



  // Exercise 15 (filter1)
object ExerciseSpec15
  extends org.scalacheck.Properties("intro"):

  property("Ex15.00: filter and filter1 are equivalent") =
    forAll { (l: List[Int], p: Int => Boolean) =>
      filter1(l, p) ?= filter(l,p) }

  // Exercise 16 (addPairwise)
object ExerciseSpec16
  extends org.scalacheck.Properties("intro"):

  property("Ex16.00: List(1,2,3) + List(4,5,6,7) == List(5,7,9)") =
    addPairwise(List(1, 2, 3), List(4, 5, 6, 7)) ?= List(5, 7, 9)

  // we cast back to scala because get is not implemented for our lists
  property("Ex16.01: addPairwise commutes with access, (l+r)(i)==l(i)+l(i)") =
    forAll (nonEmptyListOf[Int], nonEmptyListOf[Int]) { (l, r) =>
        val n = length(l).min (length(r)) - 1
        forAll (Gen.choose(0, n)) { i =>
          toScala(addPairwise(l, r))(i) ?= toScala(l)(i) + toScala(r)(i)
    } }

  property("Ex16.02: addPairwise with Nil is Nil [right]") =
    forAll { (l: List[Int]) => addPairwise(l, Nil) ?= Nil }

  property("Ex16.03: addPairwise with Nil is Nil [left]") =
    forAll { (l: List[Int]) => addPairwise(Nil, l) ?= Nil }

  property("Ex16.03: Summing two lists of zeroes gives a list of zeroes") =
    forAll { (l: List[Int], r: List[Int]) =>
      val l1 = map(l, _ => 0)
      val r1 = map(r, _ => 0)
      val lr = addPairwise(l1, r1)
      (lr == l1 || lr == r1) :| "a list of zeroes" &&
      (length(lr) == (length(l1).min(length(r1)))) :| "size of the shorter list"
    }



  // Exercise 17 (zipWith)
object ExerciseSpec17
  extends org.scalacheck.Properties("intro"):

  property("Ex17.00: zipWith with plus is addPairwise") =
    forAll { (l: List[Int], r: List[Int]) =>
       zipWith(l, r, _ + _) ?= addPairwise(l,r) }

  // we cast back to scala because get is not implemented for our lists
  property("Ex17.01: zipWith & get commute zipWith(l,r,f)(i)==f(l(i),l(i))") =
    forAll (nonEmptyListOf[Int], nonEmptyListOf[Int]) { (l, r) =>
      forAll { (f: (Int, Int) => Int) =>
        val n = length(l).min (length(r)) - 1
        forAll (Gen.choose(0, n)) { i =>
          toScala(zipWith(l, r, f))(i) ?= f(toScala(l)(i), toScala(r)(i))
    } } }

  property("Ex17.02: zipWith with Nil is Nil [right]") =
    forAll { (l: List[Int], f: (Int, Int) => Int) =>
      zipWith(l, Nil, f) ?= Nil }

  property("Ex17.03: zipWith with Nil is Nil [left]") =
    forAll { (l: List[Int], f: (Int, Int) => Int) =>
      zipWith(Nil, l, f) ?= Nil }



  // Exercise 18 (hasSubsequence)
object ExerciseSpec18
  extends org.scalacheck.Properties("intro"):

  property("Ex18.00: test cases from the exercise") =
    val l = List(1, 2, 3, 4)
    { hasSubsequence(l, List(1,2)) } :| "1,2,3,4 has subsequence 1,2" &&
    { hasSubsequence(l, List(2,3)) } :| "1,2,3,4 has subsequence 2,3" &&
    { hasSubsequence(l, List(4))   } :| "1,2,3,4 has subsequence 4"

  property("Ex18.01: An empty list have no non-Nil subsequence") =
     forAll (nonEmptyListOf[Int]) { (l: List[Int]) =>
       !hasSubsequence (Nil, l) }

  property("Ex18.02: Nil is a subsequence of every list") =
    forAll { (l: List[Int]) => hasSubsequence (l, Nil) }

  property("Ex18.03: Concatenation introduces a subsequence") =
    forAll { (l1: List[Int], l2: List[Int]) =>
      { hasSubsequence(concat(List(l1, l2)), l2)     } :| "suffix" &&
      { hasSubsequence(concat(List(l1, l2)), l1)     } :| "prefix" &&
      { hasSubsequence(concat(List(l1, l2, l1)), l2) } :| "midfix"
    }
