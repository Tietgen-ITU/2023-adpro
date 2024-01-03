/* These tests have not been available for the students writing the exam. */
package adpro.solution

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import fpinscala.answers.state.RNG
import fpinscala.answers.laziness.LazyList
import fpinscala.answers.laziness.LazyList.*

object Exam2021AutumnSpec
  extends org.scalacheck.Properties("exam-2021"):

  // Q1

  import Q1.Printable.*
  import Q1.Printable

  property("    A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  property("Q1: Warm-up hello") =
    Q1.hello(Triangle) == "triangle"
      && Q1.hello(Triangle: Printable) == "triangle"
      && Q1.hello(Square) == "square"
      && Q1.hello(Square: Printable) == "square"

  // Q2

  property("Q2: sequence and either #1") =
    Q2.sequence(List()) == Right(List())

  property("Q2: sequence and either #2") =
    Q2.sequence(List(Left(()))) == Left(())

  property("Q2: sequence and either #3") =
    Q2.sequence(List(Left(1), Left(2))) == Left(2)

  property("Q2: sequence and either #4") =
    forAll { (l: List[Int]) =>
      Q2.sequence(l.map { Right.apply }) == Right (l) }

  // Q4

  property("Q4: Random Value Generators") =
    forAll { (seed: Long) =>
      val (l, r, x) = Q4.riid.run(RNG.Simple(seed))._1
      l <= r && l.toDouble <= x && x <= r.toDouble
    }


  // Q8

  property("Q8: (A better size for streams)") =
    val s = cons (???, cons(???, LazyList.empty))
    Q8.checkIfLongerEqThan(s)(0)
      && Q8.checkIfLongerEqThan(s)(1)
      && Q8.checkIfLongerEqThan(s)(2)
      && !Q8.checkIfLongerEqThan(s)(3)
      && Q8.checkIfLongerEqThan_(s)(0)
      && Q8.checkIfLongerEqThan_(s)(1)
      && Q8.checkIfLongerEqThan_(s)(2)
      && !Q8.checkIfLongerEqThan_(s)(3)


  // Q9

  property("Q12: (Monads) (also tests Q13)") = 
    forAll { (l: List[Int]) => Q12.sum(l) == l.sum }
