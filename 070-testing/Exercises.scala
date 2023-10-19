// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Prop.*
import org.scalacheck.Arbitrary.arbitrary

// import lazyList00.* // uncomment to test the book laziness solution implementation
// import lazyList01.* // uncomment to test the broken headOption implementation
import lazyList02.* // uncomment to test another version

/* Generators and helper functions */

import LazyList.*
import scala.runtime.LazyInt

/** Convert a strict list to a lazy-list */
def list2lazyList[A](la: List[A]): LazyList[A] = 
  LazyList(la*)

/** Generate finite non-empty lazy lists */
def genNonEmptyLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] =
  for la <- arbitrary[List[A]].suchThat { _.nonEmpty }
  yield list2lazyList(la)

def genThrowingCompLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] = 
  for {
    e <- Gen.frequency(10 -> true, 1 -> false)
    v1 <- arbitrary
    ls <- if e then 
            genThrowingCompLazyList 
          else 
            Gen.frequency[LazyList[A]](1 -> Empty, 10 -> Cons(() => ???, () => ???))
  } yield Cons(() => v1, () => ls)


def genThrowingCompHeadLazyList[A](using Arbitrary[A]): Gen[LazyList[A]] = 
  for {
    e <- Gen.frequency(10 -> true, 1 -> false)
    v1 <- arbitrary
    ls <- if e then 
            genThrowingCompHeadLazyList 
          else 
            Gen.lzy(empty)
  } yield Cons(() => v1, () => ls)

def genThrowingCompLazyListWithcons[A](using Arbitrary[A]): Gen[LazyList[A]] = 
  for {
    e <- Gen.frequency(10 -> true, 1 -> false)
    v1 <- arbitrary
    ls <- if e then 
            genThrowingCompLazyListWithcons 
          else 
            Gen.frequency[LazyList[A]](1 -> Empty, 10 -> cons(???, ???))
  } yield cons(v1, ls)

def genNonNegativeInt[A](using Arbitrary[A]): Gen[Int] =
  arbitrary[Int].suchThat(_ != Int.MinValue).map(_.abs) 

/** Generate an infinite lazy list of A values.
  *
  * This lazy list is infinite if the implicit generator for A never fails. The
  * code is ugly-imperative, but it avoids stack overflow (as Gen.flatMap is
  * not tail recursive)
  */
def infiniteLazyList[A: Arbitrary]: Gen[LazyList[A]] =
  def loop: LazyList[A] =
    summon[Arbitrary[A]].arbitrary.sample match
      case Some(a) => cons(a, loop)
      case None => empty
  Gen.const(loop)

/* The test suite */

object LazyListSpec 
  extends org.scalacheck.Properties("testing"):

  // Exercise 1

  property("Ex01.01: headOption returns None on an empty LazyList") = 
    empty.headOption == None

  property("Ex01.02: headOption returns the head of the stream packaged in Some") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int) => cons(n,empty).headOption == Some(n) } :| "singleton" &&
    forAll { (s: LazyList[Int]) => s.headOption != None }      :| "random" 

  property("Ex01.03: cons returns a non-empty lazylist") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingCompLazyListWithcons[Int])

    forAll {(xs: LazyList[Int]) => xs != Empty}


  // Exercise 2
  property("Ex02.01: headOption does not force computation of tail") =

    forAll { (n: Int) => cons(n, ???).headOption == Some(n) }
  
  // Exercise 3
  property("Ex03.01: take does not force any computations") = 

    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingCompLazyList[Int])

    forAll { (n: Int, xs: LazyList[Int]) => 
      n > 0 ==> ({ 
        xs.take(n)
        true
      })}

  // Exercise 4
  property("Ex04.01: take does not force next head") = 

    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingCompLazyList[Int]) 

    forAll {(n: Int, xs: LazyList[Int]) => n > 0 ==>
      ({xs.take(n+1) ; true})}
  
  // TODO: Test that this terminates if the tail is of type empty
  // TODO: Test that this terminates after n
  
  // Exercise 5
  property("Ex05.01: take(n) performed twice yields the same result as doing it once") =
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll { (n: Int, xs: LazyList[Int]) => n > 0 ==> 
      ({
        xs.take(n).take(n).toList == xs.take(n).toList
      })}
  
  // Exercise 6
  property("Ex06.01: drop n and drop m is the same as drop n+m") =

    given Arbitrary[Int] = Arbitrary(genNonNegativeInt[Int])
    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll {(n:Int, m:Int, xs: LazyList[Int]) => n+m >=0 ==>
      ({
        xs.drop(n).drop(m).take(1).toList == xs.drop(n+m).take(1).toList
      })}
  
  // Exercise 7
  property("Ex07.01: drop does not force computation on head") =

    given Arbitrary[Int] = Arbitrary(genNonNegativeInt[Int])
    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingCompHeadLazyList[Int])

    forAll((n:Int, xs:LazyList[Int]) => { xs.drop(n) ; true})

  // Exercise 8
  property("Ex08.01: map with the identity function provides the same list") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])
    forAll {(xs:LazyList[Int]) => 
      (xs.map(a => a).toList == xs.toList)}

  // Exercise 9

  property("Ex09.01: map terminates even on infinite lists") =

    given Arbitrary[LazyList[Int]] = Arbitrary(infiniteLazyList[Int])

    forAll { (xs: LazyList[Int]) => {xs.map(a => a) ; true}}
 
  // Exercise 10

  property("Ex10.01: empty append non-empty list returns the list that is not empty") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll ((xs: LazyList[Int]) => xs.append(empty) != empty)
  
  property("Ex10.02: empty appended with non-empty list returns the non-empty list") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll ((xs: LazyList[Int]) => xs.append(empty).toList == xs.toList)
  
  property("Ex10.03: append two lists has the two list appended in order") =

    given Arbitrary[LazyList[Int]] = Arbitrary(genNonEmptyLazyList[Int])

    forAll((xs:LazyList[Int]) => {
      val appended = xs.append(xs)
      val xsLen = xs.toList.length

      xs.toList == appended.take(xsLen).toList 
    })

  property("Ex10.04: does not force next head nor tail") = 

    given Arbitrary[LazyList[Int]] = Arbitrary(genThrowingCompLazyList[Int])

    forAll ((xs: LazyList[Int]) => { xs.append(empty) ; true })

