// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

package adpro.monad

import org.scalacheck.{Arbitrary, Prop, Test, Gen}
import org.scalacheck.Prop.*
import org.scalactic.Equality

object MonoidSpec
  extends org.scalacheck.Properties("monoid"):

  // Pre Exercises. Monoid tests for strings and lists
 
  property("Ex04.01: stringMonoid is a monoid") =
    stringMonoid.laws.monoid
 
  property("Ex04.01: listMonoid[Char] is a monoid") =
    listMonoid[Char].laws.monoid

  // Exercise 1 (intAddition, intMultiplication, booleanOr, booleanAnd

  property("Ex01.01: Not a test, just a msg---tests are found in Ex.4") = true

  // Exercise 2 (Monoid[Option[A]]

  property("Ex02.01: Not a test, just a msg---tests are found in Ex.4") = true

  // Exercise 3 (endMonoid)

  property("Ex03.01: endoMonoid is a monoid") =

    /* In Scala, like in most langauges, functions cannot be compared for
     * equality. A reference equality test between two function values gives
     * false, unless it is really the same thunk we are comparing.  To create an
     * equality test, we 'cheat' by replacing a hard equality test by a property
     * test for equality based on random sampling.
    
     * Override equality used for functions to use in monoid laws below
     * (picked up by implicit parameters) */
    given intFnEq: Equality[Int => Int] = new Equality[Int => Int]:
      def areEqual(f: Int => Int, g: Any): Boolean =
        try // Nasty but I'm tired of fighting type erasures on JVM
          val p = forAll { (x: Int) => f(x) == g.asInstanceOf[Int => Int](x) }
          Test.check(Test.Parameters.default, p).passed
        catch e => false
    endoMonoid[Int].laws.monoid
 
  // Exercise 5 (foldMap)

  property("Ex05.01: foldMap a constant list with identity") =
    forAll { (l: List[Int], n: Int) =>
      intAddition.foldMap (l.map {_ => n})(identity[Int]) == l.size * n }

  property("Ex05.02: foldMap a constant list with incr") =
    forAll { (l: List[Int], n: Int) =>
      intAddition.foldMap (l.map {_ => n}) { _ + 1 } == l.size * (n+1) }

  // Exercise 6 

  property("Ex06.01: Not a test, just a msg---tests are found in Ex.7") = true

end MonoidSpec



object FoldableSpec
  extends org.scalacheck.Properties("foldab"):

  // Exercise 11 (FoldableList)

  property("Ex11.01: Sum a list using a Foldable instance and foldLeft") =
    forAll { (l: List[Int], k: Int) =>
      (l.foldLeft(k) { _ + _ }) == (l.sum + k) }

  property("Ex11.02: Sum a list using a Foldable instance and foldRight") =
    forAll { (l: List[Int], k: Int) =>
      (l.foldRight(-k) { _ + _ }) == (l.sum - k) }

  property("Ex11.03: size a list using a Foldable instance with foldMap") =
    forAll { (l: List[Unit]) =>
      (l.foldMap { _ => 1 } (using intAddition)) == (l.size) }

  // Exercise 12 (toList)

  /* This weird test foldable is just a unit, folding over it always gives empty,
   * so toList should give an emptyList. */

  opaque type UnitFoldable[A] = Unit
  val foldableUnit: UnitFoldable[Int => Int] = ()

  given Foldable[UnitFoldable] = new:
    extension [A] (as: UnitFoldable[A]) 
      def foldMap[B](f: A => B) (using mb: Monoid[B]): B = mb.empty

  property("Ex12.01: toListF on UnitFoldable gives empty list") =
    foldableUnit.toListF == Nil

  /* This weird test foldable is a pair of an integer n and a value A, and it
   * behaves as a list List.fill(n)(a) */

  opaque type IntFoldable[A] = (Int, A)

  given Foldable[IntFoldable] = new:
    extension [A] (na: IntFoldable[A]) 
      def foldMap[B](f: A => B)(using mb: Monoid[B]): B = 
        (0 to (na._1 -1))
          .map { _ => f(na._2) }
          .foldRight(mb.empty)(mb.combine)

  property("Ex12.02: toListF on intFoldable gives List.fill(n)(a)") =
    forAll { (n: Int, a: Double) =>
      val n1 = (n % 100).abs
      val result: List[Double] = (n1, a).toListF
      val expect: List[Double] = List.fill(n1)(a) 
      (result == expect) :| s"got:$result expected:$expect"
    }
        
end FoldableSpec


object FunctorSpec
  extends org.scalacheck.Properties("functo"):

  // Exercise 13

  property("Ex13.01: Not a test, just a msg---tests are found in Ex.14") = true

  // Exercise 14

  property("Ex14.01: Not a test, just a msg---tests are found in Ex.15") = true

end FunctorSpec

object MonadSpec
  extends org.scalacheck.Properties("monad_"):

  // Exercise 15

  property("Ex15.01: Not a test, just a msg---tests are found in Ex.16") = true


  // Exercise 17 (sequence)

  /** Identity Monad, the simplest monad, good for testing */
  opaque type Id[A] = A
  given Monad[Id] = new:
    def unit[A] (a: => A): Id[A] = a
    extension [A](a: Id[A])
      def flatMap[B](f: A => Id[B]): Id[B] = f(a)

  property("Ex17.01: sequence in idMonad (1)") =
    forAll { (l: List[Int]) => summon[Monad[Id]].sequence[Int](l) == l }

  property("Ex17.02: sequence in idMonad (2)") =
    forAll { (l: List[Unit]) => summon[Monad[Id]].sequence[Unit](l) == l }

  property("Ex17.03: sequence in optionMonad (1)") =
    forAll { (l: List[Int]) =>
      val oas = l.map { Some (_) }
      optionMonad.sequence(oas) == Some(l) }

  property("Ex17.04: sequence in optionMonad (2)") =
    forAll { (n: Int) =>
      val oas = List.fill(Math.abs(n % 100) + 1)(None)
      optionMonad.sequence(oas) == None }


  // Exercise 18 (replicateM)

  property("Ex18.01: replicateM in Id") =
    forAll(Gen.choose(0, 100)) { (n: Int) =>
      summon[Monad[Id]].replicateM(n, n) == List.fill(n)(n) }

  property("Ex18.02: replicateM in Option") =
    forAll (Gen.choose(0, 100)) { (n: Int) =>
      optionMonad.replicateM(n+1, None) == None }

  // Exercise 19 (Kleisli)

  property("Ex19.01: Kleisli composition in Id") =
    forAll { (f: Int => Id[Int], g: Int => Id[String], x: Int) =>
      summon[Monad[Id]].compose[Int, Int, String](f, g)(x) == g (f (x)) }

  property("Ex19.02: Kleisli composition in Option") =
    forAll { (f: Int => Int, g: Int => String, x: Int) =>
      val fo: Int => Option[Int] = x => Some(f(x))
      val go: Int => Option[String] = x => Some(g(x))
      optionMonad.compose[Int, Int, String](fo, go)(x) == Some(g(f(x))) }

end MonadSpec

