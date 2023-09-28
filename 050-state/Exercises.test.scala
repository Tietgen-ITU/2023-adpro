// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import RNG.*

// Helper functions

// Slightly cheating; Uses another generator to seed ours
def genRNG (using arbLong: Arbitrary[Long]): Gen[RNG] =
  for seed <- arbLong.arbitrary 
  yield SimpleRNG(seed)

given Arbitrary[RNG] = Arbitrary(genRNG)



/** Check if the given generator is constant */
def generatorIsNotConstant[A](f: RNG => (A, RNG), n: Int = 20): Prop = forAll { 
  (rng: RNG) =>
    val (randomAs, _) =
      List.fill(n) (())
        .foldLeft[(List[A], RNG)](Nil, rng) { 
          case ((l, rng), _) => (f(rng)._1:: l, f(rng)._2) }
    randomAs.toSet.size > 1
  }

// A type-cast version of ints
val ii = { (n: Int) => ints(n).asInstanceOf[RNG => (List[Int], RNG)] }


object RNGSpec
    extends org.scalacheck.Properties ("rng"):

  import RNG.*

  // Exercise 1 (nonNegativeInt)

  property("Ex01.01: The generated integers are non-negative") =
    forAll { (rng: RNG) => nonNegativeInt (rng)._1 >= 0 }
   
  property("Ex01.02: nonNegativeInt is not a constant generator") =
    generatorIsNotConstant(nonNegativeInt)

  // Exercise 2 (double)

  property("Ex02.01: Generated doubles are greater or equal than zero") =
    forAll { (rng: RNG) => double(rng)._1 >= 0.0 }

  property("Ex02.02: Generated doubles are less than one") =
    forAll { (rng: RNG) => double(rng)._1 < 1.0 }

  property("Ex02.03: The double generator is not constant") = 
    generatorIsNotConstant(double)
   
  // Exercise 3 (intDouble, DoubleInt)

  property("Ex03.01: You got the return type right for intDouble") =
    intDouble(SimpleRNG(42)).asInstanceOf[((Int,Double),RNG)]
    true

  property("Ex03.02: You got the return type right for intDouble") =
    doubleInt(SimpleRNG(42)).asInstanceOf[((Double,Int),RNG)]
    true

  property("Ex03.03: The intDouble generator is not constant") = 
    generatorIsNotConstant(intDouble.asInstanceOf[RNG => ((Int, Double), RNG)])

  property("Ex03.04: The doubleInt generator is not constant") = 
    generatorIsNotConstant(doubleInt.asInstanceOf[RNG => ((Double, Int), RNG)])

  property("Ex03.05: The integers from intDouble are non-negative") = 
    forAll{ (rng: RNG) => 
      intDouble.asInstanceOf[RNG => ((Int, Double), RNG)](rng)._1._1 >= 0 }

  property("Ex03.06: The integers from doubleInt are non-nogative") = 
    forAll{ (rng: RNG) => 
      doubleInt.asInstanceOf[RNG => ((Double, Int), RNG)](rng)._1._2 >= 0 }
 
  // Exercise 4 (ints)

  property("Ex04.01: You got the return type right for intDouble") =
    ints(100)(SimpleRNG(42)).asInstanceOf[(List[Int],RNG)]
    true

  // Avoid shrinking because many buggy solutions would enter an
  // infinite loop if the shrinker gives them a negative value
  // https://gist.github.com/davidallsopp/f65d73fea8b5e5165fc3
  property("Ex04.02: ints should not throw exceptions") =
    forAllNoShrink (genRNG, Gen.choose(0, 2000)) { (rng: RNG, size: Int) => 
      ints(size).asInstanceOf[RNG => (List[Int], RNG)](rng); true }

  // Avoid shrinking because many buggy solutions would enter an
  // infinite loop if the shrinker gives them a negative value
  property("Ex04.03: The created lists should have the specified length") =
    forAllNoShrink (genRNG, Gen.choose(0, 2000)) { (rng: RNG, size: Int) =>
      ints(size).asInstanceOf[RNG => (List[Int], RNG)](rng)._1.size == size 
    }

  property("Ex04.04: The ints generator is not constant") = 
    generatorIsNotConstant(ints(5).asInstanceOf[RNG => (List[Int], RNG)])

  // Avoid shrinking because many buggy solutions would enter an
  // infinite loop if the shrinker gives them a negative value
  property("Ex04.05: The created lists are not constant") =
    forAllNoShrink (genRNG, Gen.choose(20, 200)) { (rng: RNG, size: Int) =>
      val (l, _) = ints(size).asInstanceOf[RNG => (List[Int], RNG)](rng)
      l.toSet.size > 1
    }
  
  // Exercise 5 (_double)

  property("Ex05.01: double2 and double should behave the same") =
    forAll { (rng: RNG) => double(rng) == double2(rng) }

  // TODO: one day use macros to check if map is used

  // Exercise 6 (map2)

  property("Ex06.01: map2 should give an equivalent intDouble") =
    forAll { (rng: RNG ) =>
      val intDoubleMap2 = map2(nonNegativeInt, double) { (_,_) }
      intDoubleMap2(rng) == intDouble(rng)
    }

  property("Ex06.02: map2 can emulate map") =
    forAll { (rng: RNG, f: ((Double, Int)) => Int) =>
      val dI = doubleInt.asInstanceOf[RNG => ((Double,Int), RNG)]
      map(dI)(f)(rng) == map2(dI, unit(42))((di, _) => f(di._1, di._2))(rng)
    }

  // Exercise 7 (sequence)

  property("Ex07.01: A simple fixed scenario (sequencing a const. generator)") =
    forAll { (rng: RNG, l: List[Int]) =>
      val r = sequence (l.map { unit })
      r(rng)._1 == l
    }

  property("Ex07.02: Sequencing an empty list gives an empty list") =
    forAll { (rng: RNG) => sequence(List[Rand[Int]]())(rng)._1 == Nil }

  // It is easier to change the order in ints, so try to swap the generation 
  // order in ints, if this fails. The order in ints2 is fixed by map2 
  // and foldRight
  property("Ex07.03: ints2 & ints are the same (needs sync btw ints & ints2)") =
    forAllNoShrink (genRNG, Gen.choose(0, 2)) { (rng, size) => 
      ints(size)(rng) == ints2(size)(rng) }

  // Exercise 8 (flatMap, nonNegativeLessThan)

  property("Ex08.01: flatMap associativity law") =
    forAll { (f: Int => Int, g: Int => Int, rng: RNG) => 
      val r1 =
        flatMap(flatMap(nonNegativeInt) { n => unit(f(n)) }) { m => unit(g(m)) }
      val r2 =
        flatMap(nonNegativeInt) { n => flatMap(unit(f(n))) { m => unit (g(m)) } }
      r1(rng) == r2(rng)
    }

  property("Ex08.02: flatMap identity law (right)") =
    forAllNoShrink (Gen.choose(0, 200), genRNG) { (n, rng) =>
      val r1 = flatMap(ii(n)) { unit }
      val r2 = ii(n)
      r1(rng) == r2(rng)
    }

  property("Ex08.03: flatMap identity law (left)") =
    forAllNoShrink (Gen.choose(0, 200), genRNG) { (n, rng) =>
      val r1 = flatMap(unit(n)) { ii }
      val r2 = ii (n)
      r1(rng) == r2(rng)
    }

  property("Ex08.04: nonNegativeLessThan(n) yields numbers >= 0") =
    forAllNoShrink (Gen.choose(1, 10000), genRNG) { (n, rng) =>
      nonNegativeLessThan(n)(rng)._1 >= 0
    }

  property("Ex08.05: nonNegativeLessThan(n) yields numbers < n") =
    forAllNoShrink (Gen.choose(1, 10000), genRNG) { (n, rng) =>
      nonNegativeLessThan(n)(rng)._1 <= n
    }

end RNGSpec



object StateSpec
    extends org.scalacheck.Properties("state"):

  import State.*
  // Exercise 9

  property("Ex09.01: unit produces one action and has one state") =
    forAll { (n: Int, m: Int) =>
      val s: State[Int,Int] = unit(n)
      s.run(m) == (n, m)
    }

  property("Ex09.02: map identity law") =
    forAll  { (n: Int, m: Int) =>
      val s = State[Int, String](k => (s"[$k]", k+m))
      s.run(n) == s.map { identity }.run(n)
    }

  property("Ex09.03: associativity flatMap") =
    forAll { (rng: RNG) =>
      val s1 = State[RNG,Int](RNG.nonNegativeInt)
        .flatMap { n => unit(n + 1)  }
        .flatMap { m => unit(m - 42) }
      val s2 = State[RNG,Int](RNG.nonNegativeInt)
        .flatMap { n => unit(n + 1)
        .flatMap { m => unit(m - 42) } }
      s1.run(rng) == s2.run(rng)
    }

  property("Ex09.04: flatMap identity law (right)") =
    forAllNoShrink (Gen.choose(0, 1000), genRNG) { (n, rng) =>
      val s1 = State[RNG, List[Int]](ii(n)).flatMap { unit }
      val s2 = State[RNG, List[Int]](ii(n))
      s1.run(rng) == s2.run(rng)
    }

  property("Ex09.05: flatMap identity law (left)") =
    forAllNoShrink (Gen.choose(0, 1000), genRNG) { (n, rng) =>
      val s1 = unit(n).flatMap { n => State(ii(n)) }
      val s2 = State(ii(n))
      s1.run(rng) == s2.run(rng)
    }

  property("Ex09.06: map is the same as flatMap (unit compose f)") =
    forAll { (n: Int, m: Int) =>
      val s1 = State[Int,String] { k => (s"[$k]", k+m) }
        .map { m => s"${m}_42" }
      val s2 = State[Int,String] { k => (s"[$k]", k+m) }
        .flatMap { m => unit(s"${m}_42") }
      s1.run(n) == s2.run(n)
    }

  // If this is failing it is likely that `intDouble` does things in wrong order
  // Revisit its implementation, when debugging.
  property("Ex09.07: map2 gives same intDouble as the direct implementation") =
    forAll { (rng: RNG) =>
      val intDouble = 
        State(RNG.nonNegativeInt).map2 (State(RNG.double)) { (_, _) }
      intDouble.run(rng) == RNG.intDouble(rng)
    }

  property("Ex09.08: Sequencing a list of units gives a unit of same list") =
    forAll { (rng: RNG, l: List[Int]) =>
      val s = State.sequence(l.map { n => unit[RNG,Int] (n) })
      s.run(rng)._1 == l
    }

  import adpro.lazyList.LazyList
  
  // Exercise 10 (stateToLazyList)

  property("Ex10.01: A constant automaton generates a constant lazy list") =
    forAll { (n: Int, m: Int) =>
      val s = unit[Int, Int](n)
      stateToLazyList[Int, Int](s)(m).take(1000).toList == List.fill(1000)(n)
    }

  property("Ex10.02: An increment automaton generates a list of naturals") =
    val s = State[Int, Int] { x => (x, x+1) }
    stateToLazyList[Int, Int](s)(0).take(1000).toList 
      == List.tabulate(1000)(identity[Int])

  property("Ex10.03: An automaton cannot generate an empty lazy list") =
    forAll { (run: Int => (Int, Int)) =>
      stateToLazyList[Int, Int](State(run)) != LazyList.Empty }

  // Exercise 11 (lazyInts, ten strict ints)

  property("Ex11.01: No exception should be thrown by lazyInts") =
    forAll { (rng: RNG) => lazyInts(rng).take(1000).toList; true }

  property("Ex11.02: The random integers seem random") =
    forAll { (rng: RNG) =>
      val l1 = lazyInts(rng).take(5)
      val l2 = lazyInts(rng).drop(4000).take(5)
      l1.toList != l2.toList
    }
  property("Ex11.03: The random integers are deterministic for the same seed") =
    forAll { (rng: RNG) =>
      lazyInts(rng).take(100).toList == lazyInts(rng).take(100).toList }

  property("Ex11.04: The random integers differ for different seeds") =
    forAll { (rng1: RNG, rng2: RNG, rng3: RNG) =>
      val l1 = lazyInts(rng1).take(100).toList 
      val l2 = lazyInts(rng2).take(100).toList 
      val l3 = lazyInts(rng3).take(100).toList 
      rng1 == rng2 || l1 != l2 || rng2 == rng3 || l2 != l3 
        || rng1 == rng3 || l1 != l3
    }

  property("Ex11.05: The 10 strict integers are different") =
    tenStrictInts.toSet.size > 1

  property("Ex11.06: There is 10 strict integers") =
    tenStrictInts.size == 10

end StateSpec
