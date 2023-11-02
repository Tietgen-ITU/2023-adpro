// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.prop

import adpro.state.RNG

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

// To enable all the extensions for Gen (they are in the Gen objects)
import Gen.*

object PropSpec
  extends org.scalacheck.Properties("prop"):

  /** In this test suite, we are using scalacheck to test our own property
    * based testing framework. The two libraries are super similar. They even
    * follow the same type names.  This raises a lot of confusion.  We prefix
    * the scalacheck type names with sc to disambiguate.
    */
  import org.{scalacheck as sc}
  import org.scalacheck.Prop.propBoolean
  
  // cheating slightly as we are using another generator to seed ours
  val genRNG = for n <- arbitrary[Long] yield RNG.Simple(n)
  given arbRNG: Arbitrary[RNG] =
    Arbitrary { genRNG }
 
  // Exercise 1 (RNG)"

  property("Ex01.01: One test :) [scenario]") =
    rng1 == RNG.Simple (42)

  // Exercise 2 (nextInt)

  property("Ex02.01: The first integer (x)") =
    x == rng1.nextInt._1

  property("Ex02.02: The second integer (y)") =
    y == rng1.nextInt._2.nextInt._1

  // Exercise 3 (minimum)
 
  def nonePasses(p1: sc.Prop, p2: sc.Prop): sc.Prop =
    ! sc.Test.check (sc.Test.Parameters.default, p1).passed
      && ! sc.Test.check (sc.Test.Parameters.default, p2).passed

  def onePasses(p1: sc.Prop, p2: sc.Prop): sc.Prop =
    val b1 = sc.Test.check (sc.Test.Parameters.default, p1).passed
    val b2 = sc.Test.check (sc.Test.Parameters.default, p2).passed
    (b1 || b2) && !(b1 && b2)

  def goodMinimum (l: List[Int]): Int = l.min
  def constMinimum (l: List[Int]): Int = 42
  def tooSmallMin (l: List[Int]): Int = l.min - 1
  def tooBigMin (l: List[Int]): Int = l.min + 1

  def p1 = Exercise_3.p1Min
  def p2 = Exercise_3.p2Min
  
  property("Ex03.01: Both properties should pass on correct mininmum") =
    p1(goodMinimum) && p2(goodMinimum)

  property("Ex03.02: No property should pass on constant minimum function") =
    nonePasses(p1(constMinimum), p2(constMinimum)) 

  property("Ex03.03: One property should pass with too small minimum") =
    onePasses(p1(tooSmallMin), p2(tooSmallMin)) 

  property("Ex03.04: No property should pass with too large minimum") =
    nonePasses(p1(tooBigMin), p2(tooBigMin)) 

  // Exercise 4 (&&)

  import Exercise_4.{Prop as P4}
  val ex4F = new P4 { def check = false }
  val ex4T = new P4 { def check = true }

  property("Ex04.01: two successes are a success") = 
    ex4T.&& (ex4T).check 
  property("Ex04.02: both failures are failure") = 
    ! ex4F.&& (ex4F).check 
  property("Ex04.03: left failure is failure") = 
    ! ex4F.&& (ex4T).check 
  property("Ex04.04: right failure is failure") =
    ! ex4T.&& (ex4F).check 

  // Exercise 5 (choose)

  property("Ex05.01: All Gen.choose(l, r) numbers are ≥ l") =
    sc.Prop.forAll (sc.Gen.choose (0, 10000), sc.Gen.choose (0, 10000), genRNG) { 
      (l: Int, n: Int, rng: RNG) =>
        Gen.choose (l, l + n + 1)
          .toLazyList(rng)
          .take(1000)
          .forall { _ >= l }
    }

  property("Ex05.02: All Gen.choose(l, r) numbers are < r") =
    sc.Prop.forAll (sc.Gen.choose (0, 10000), sc.Gen.choose (0, 10000), genRNG) { 
      (l: Int, n: Int, rng: RNG) =>
        Gen.choose (l, l + n + 1)
          .toLazyList(rng)
          .take(100)
          .forall { _ < l + n + 1 }
    }

  property("Ex05.03: Gen.choose(n, n+1) terminates") =
    sc.Prop.forAll (sc.Gen.choose (0, 10000), genRNG) { (n: Int, rng: RNG) =>
      Gen.choose(n, n + 1)
        .toLazyList(rng)
        .take(100)
        .forall { _ => true }
    }
 
  // Exercise 6 (unit, boolean, and double)

  property("Ex06.01: Gen.unit(c) gives a constant stream") =
    sc.Prop.forAll { (d: Double, rng: RNG) =>
      Gen.unit(d)
        .toLazyList(rng)
        .take(500)
        .forall { _ == d }
    }

  // If this fails, maybe Gen.choose is broken
  property("Ex06.02: Gen.unit(n)==Gen.choose(n,n+1)") =
    sc.Prop.forAll (sc.Gen.choose(0, 1000), genRNG) { (n: Int, rng: RNG) =>
      val ch = Gen.choose(n, n+1).toLazyList(rng)
      Gen.unit(n)
        .toLazyList(rng)
        .zip(ch)
        .take(100)
        .forall { _ == _ }
    }

  property("Ex06.03: Gen.boolean gives true eventually ") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.boolean
        .toLazyList(rng)
        .take(5000)
        .exists(identity)
    }

  property("Ex06.04: Gen.boolean gives false eventually ") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.boolean
        .toLazyList(rng)
        .take(5000)
        .exists { ! _ }
    }

  property("Ex06.05: Gen.double generates some negative numbers") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.double
        .toLazyList(rng)
        .take(5000)
        .exists { _ < 0 }
    }

  property("Ex06.06: Gen.double generates some positive numbers") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.double
        .toLazyList(rng)
        .take(5000)
        .exists { _ > 0 }
    }

  property("Ex06.07: gen.double generates numbers smaller than 1.0") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.double
        .toLazyList(rng)
        .take(5000)
        .exists { _ <= 1.0 }
    }
  property("Ex06.08: gen.double generates numbers greater than 1.0") =
    sc.Prop.forAll { (rng: RNG) =>
      Gen.double
        .toLazyList(rng)
        .take(5000)
        .exists { _ > 1.0 }
    }
  
  // Exercise 7 (listOfN)

  property("Ex07.01: listOfN(n) give a list of size n") =
    sc.Prop.forAll(sc.Gen.choose(0,100), genRNG) { (n: Int, rng: RNG) =>
      Gen.unit(())
        .listOfN(n)
        .toLazyList(rng)
        .take(50)
        .forall { _.size == n }
    }

  property("Ex07.02: unit(c).listOfN(n) gives a list of c's") =
    sc.Prop.forAll(sc.Gen.choose(0, 100), sc.Gen.double, genRNG) { 
      (n: Int, c: Double, rng: RNG) =>
        Gen.unit(c)
          .listOfN(n)
          .toLazyList(rng)
          .take(100)
          .forall { l => l.forall { _ == c } }
    }

  property("Ex07.03: choose(0,r).listOfN(n) gives non-constant lists") =
    sc.Prop.forAll (sc.Gen.choose(10, 100), sc.Gen.choose(100,1000), genRNG) { 
      (n: Int, r: Int, rng: RNG) =>
        Gen.choose(0, r) 
          .listOfN(n)
          .toLazyList(rng)
          .take(50)
          .toList
          .toSet
          .size > 2
    }

  // Exercise 9 (flatMap)
  
  // Generating random Gen from scalachecks gens, he he :)
  // Not good for testing our Gens, except unit, but might be good enough
  // for testing our Gen operators.
  def genGen[A: Arbitrary]: sc.Gen[Gen[A]] = 
    for rng <- genRNG
          a <- summon[Arbitrary[A]].arbitrary
    yield Gen.unit(a)

  given arbGen [A: Arbitrary]: Arbitrary[Gen[A]] = 
    Arbitrary { genGen[A] }

  property("Ex09.01: A simple fixed flatMap scenario") =
    sc.Prop.forAll (sc.Gen.choose(0, 1000), sc.Gen.double, genRNG) { 
      (n: Int, d: Double, rng: RNG) => 
        Gen.unit(n)
          .flatMap { Gen.unit(d).listOfN }
          .toLazyList(rng)
          .take(100)
          .forall { _ == List.fill(n)(d) }
    }

  // This test also (accidentally) checks whether your Gens are deterministic
  property("Ex09.02: flatMap is associative") =
    sc.Prop.forAll { 
      (gen: Gen[Int], f: Int => Gen[Int], g: Int => Gen[Int], rng: RNG) =>
        val left = gen.flatMap(f).flatMap(g)
        val right = gen.flatMap { (n: Int) => f(n).flatMap(g) }
        left.toLazyList(rng).take(100) == right.toLazyList(rng).take(100)
    }
    
  property("Ex09.03: map is flatMap with unit") =
    sc.Prop.forAll { (gen: Gen[Int], f: Int => Int, rng: RNG) => 
      val left = gen.map(f).toLazyList(rng).take(50)
      val right = gen.flatMap { (n: Int) => Gen.unit(f(n)) }.toLazyList(rng).take(50)
      left.toList == right.toList
    }

  // Exercise 10 (listOf)

  property("Ex10.01: listOf(unit(n)) produces a list of size n") =
    sc.Prop.forAll (sc.Gen.choose(0, 100), genRNG) { (n: Int, rng: RNG) =>
      Gen.unit[Unit](())
        .listOf(Gen.unit(n))
        .toLazyList(rng)
        .take(5)
        .forall { _.size == n }
    }

  property("Ex10.02: listOf(unit(n)) can produce a list of varying sizes") =
    sc.Prop.forAll(sc.Gen.choose(20, 100), genRNG) { (n: Int, rng: RNG) =>
      Gen.unit[Int](42)
        .listOf(Gen.choose(1, n))
        .toLazyList(rng)
        .take(20)
        .map { _.size }
        .toSet
        .size > 2
    }

  // Exercise 11 (union)

  property("Ex11.01: unit(x).union(unit(x)) is unit(x)") =
    sc.Prop.forAll { (x: Double, rng: RNG) =>
      val g = Gen.unit(x)
      g.union(g).toLazyList(rng).take(10).toList 
        == g.toLazyList(rng).take(10).toList
    }

  property("Ex11.02: union of two different generators provides some values from the left") =
    sc.Prop.forAll { (x: Double, y: Double, rng: RNG) =>
      val g = Gen.unit(x).union(Gen.unit(y)).toLazyList(rng).take(100)
      x != y ==> { g.filter { _ == x }.size > 10 }
    }

  property("Ex11.03: union of two different generators provides some values from the right") =
    sc.Prop.forAll { (x: Double, y: Double, rng: RNG) =>
      val g = Gen.unit(x).union(Gen.unit(y)).toLazyList(rng).take(100)
      x != y ==> { g.filter { _ == y }.size > 10 }
    }

  // Exercise 12 (type classes, givens)

  property("Ex12.01: listOfN(n) give a list of size n given Gen.unit(())") =
    given genUnit: Gen[Unit] = Gen.unit[Unit](())
    sc.Prop.forAll(sc.Gen.choose(0,100), genRNG) { (n: Int, rng: RNG) =>
      Exercise_12.listOfN(n)
        .toLazyList(rng)
        .take(50)
        .forall { _.size == n }
    }

  property("Ex12.02: listOfN(n) gives a list of c's given unit(c)") =
    sc.Prop.forAll(sc.Gen.choose(0, 100), sc.Gen.double, genRNG) { 
      (n: Int, c: Double, rng: RNG) =>
        given genC: Gen[Double] = Gen.unit(c)
        Exercise_12.listOfN(n)
          .toLazyList(rng)
          .take(100)
          .forall { l => l.forall { _ == c } }
    }

  property("Ex12.03: listOfN(n) gives non-constant lists given choose(0, r)") =
    sc.Prop.forAll (sc.Gen.choose(10, 100), sc.Gen.choose(100,1000), genRNG) { 
      (n: Int, r: Int, rng: RNG) =>
        given Gen[Int] = Gen.choose(0, r) 
        Exercise_12.listOfN(n)
          .toLazyList(rng)
          .take(50)
          .toList
          .toSet
          .size > 2
    }

  property("Ex12.04: listOf produces a list of size n given unit[Int](n) and Gen[Unit]") =
    given Gen[Unit] = Gen.unit[Unit](())
    sc.Prop.forAll (sc.Gen.choose(0, 100), genRNG) { (n: Int, rng: RNG) =>
      given Gen[Int] = Gen.unit(n)
      Exercise_12.listOf
        .toLazyList(rng)
        .take(5)
        .forall { _.size == n }
    }

  property("Ex12.05: listOf can produce a list of varying sizes given Gen.choose and Genp[Unit]") =
    given Gen[Unit] = Gen.unit[Unit](())
    sc.Prop.forAll(sc.Gen.choose(20, 100), genRNG) { (n: Int, rng: RNG) =>
      given Gen[Int] = Gen.choose(1, n)
      Exercise_12.listOf
        .toLazyList(rng)
        .take(20)
        .map { _.size }
        .toSet
        .size > 2
    }

  // Exercise 13 (&&)
 
  def ex13T[A] (g: Gen[A]): Prop = forAll(g) { (Double) => true }
  def ex13F[A] (g: Gen[A]): Prop = forAll(g) { (Double) => false }

  // maxSize is set to -1 because it is ignored at this stage of the exercise
  property("Ex13.01: && two successes are a success") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      ! (ex13T(g) && ex13T(g)) (-1, 10, rng).isFalsified }

  property("Ex13.02: && both failures are failure") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      (ex13F(g) && ex13F(g))(-1, 10, rng).isFalsified }

  property("Ex13.03: && left failure is failure") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      (ex13F(g) && ex13T(g))(-1, 10, rng).isFalsified }

  property("Ex13.04: && right failure is failure") =
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      (ex13T(g) && ex13F(g))(-1, 10, rng).isFalsified }

  property("Ex13.05: || two successes are a success") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      ! (ex13T(g) || ex13T(g)) (-1, 10, rng).isFalsified }

  property("Ex13.06: || both failures are failure") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      (ex13F(g) || ex13F(g))(-1, 10, rng).isFalsified }

  property("Ex13.07: || left failure is success") = 
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      ! (ex13F(g) || ex13T(g))(-1, 10, rng).isFalsified }

  property("Ex13.08: || right failure is success") =
    sc.Prop.forAll { (g: Gen[Int], rng: RNG) => 
      ! (ex13T(g) || ex13F(g))(-1, 10, rng).isFalsified }

  // Exercise 14 (unsize)

  property("Ex14.01: Gen[A].unsized is Gen[A]") =
    sc.Prop.forAll (genGen[Int], sc.Gen.choose(0,100), genRNG) { 
      (g: Gen[Int], size: Int, rng: RNG) =>
        val h: Gen[Int] = g.unsized(size)
        g.unsized(size).toLazyList(rng).take(100).toList 
          == g.toLazyList(rng).take(100).toList 
    }

  // Exercise 15 (list)

  property("Ex15.01: Gen[Unit].list controls the size of the list") = 
    sc.Prop.forAll(sc.Gen.choose(0,1000), genRNG) { (n: Int, rng: RNG) =>
      Gen.unit(())
        .list(n)
        .toLazyList(rng)
        .take(50)
        .forall { _.size == n }
    }

  // Exercise 16 (test minimum)

  import SGen.*

  def nonePassesProp(p1: Prop, p2: Prop): Boolean =
    !p1.run() && !p2.run()

  def onePassesProp(p1: Prop, p2: Prop): Boolean =
    val b1 = p1.run()
    val b2 = p2.run()
    (b1 || b2) && !(b1 && b2)

  property("Ex16.01: p1Min does not crash") =
    (Exercise_16.p1Min(goodMinimum)).run()

  property("Ex16.02: p2Min does not crash") =
    (Exercise_16.p2Min(goodMinimum)).run()

  property("Ex16.03: Both properties should pass on correct mininmum") =
    (Exercise_16.p1Min(goodMinimum) && Exercise_16.p2Min(goodMinimum)).run()

  property("Ex16.04: No property should pass on constant minimum function") =
    nonePassesProp(Exercise_16.p1Min(constMinimum), Exercise_16.p2Min(constMinimum)) 

  property("Ex16.05: One property should pass with too small minimum") =
    onePassesProp(Exercise_16.p1Min(tooSmallMin), Exercise_16.p2Min(tooSmallMin)) 

  property("Ex16.06: No property should pass with too large minimum") =
    nonePassesProp(Exercise_16.p1Min(tooBigMin), Exercise_16.p2Min(tooBigMin)) 

// vim:cc=80
