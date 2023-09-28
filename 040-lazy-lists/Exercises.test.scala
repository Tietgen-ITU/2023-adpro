// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

def ones: LazyList[Int] = LazyList.cons(1, ones)

def lazyListOf[A: Arbitrary]: Gen[LazyList[A]] = 
  for l <- Gen.listOfN[A] (10, summon[Arbitrary[A]].arbitrary)
  yield LazyList(l*)

def nonEmptyLazyListOf[A: Arbitrary]: Gen[LazyList[A]] = 
  lazyListOf[A].filter { _ != LazyList.Empty }

given arbLazyListOf[A: Arbitrary]: Arbitrary[LazyList[A]] = 
  Arbitrary { lazyListOf[A] }

object LazyListSpec
  extends org.scalacheck.Properties("lazy-list"):

  import LazyList.*
 
  // Exercise 1 (from, to, naturals)

  property("Ex01.00: The first element of LazyList.from(3) is 3") = 
    from(3).headOption == Some(3)

  property("Ex01.01: The second element of LazyList.from(3) is 4") = 
    from(3).tail.headOption == Some(4)

  property("Ex01.02: The 'from' list is long (shall pass after Ex. 5 is solved)") = 
    from(0).exists { _ == 999 }

  property("Ex01.03: The 'from' list is long (shall pass after Ex. 5 is solved)") = 
    to(0).exists { _ == -999 }

  property("Ex01.04: The first element of LazyList.to(3) is 3") = 
    to(3).headOption == Some(3)

  property("Ex01.05: The second element of LazyList.to(3) is 2") = 
    to(3).tail.headOption == Some(2)

  property("Ex01.06: The first element of naturals is 1") = 
    naturals.headOption == Some(1)

  // Exercise 2 (toList)

  property("Ex02.00: The LazyList(1,2,3).toList is List(1,2,3)") = 
    cons(1, cons(2, cons(3, empty))).toList == List(1, 2, 3)

  property("Ex02.01: empty.toList is Nil") = 
    empty.toList == Nil
 
  // Exercise 3 (take)

  property("Ex03.00: naturals.take (3) is LazyList (1,2,3) ") = 
    naturals.take(3).toList == List(1, 2, 3)

  property("Ex03.01: naturals.drop (3) is LazyList (4,5,6,...) ") = 
    naturals.drop(3).take(3).toList == List(4, 5, 6)

  property("Ex03.02: naturals.take(10^9).drop(41).take(10).toList.size = 10") =
    naturals.take(1000000000).drop(41).take(10).toList.size == 10

  property("Ex03.03: empty.take(1000000000) is empty") =
    empty.take(1000000000).toList == Nil

  property("Ex03.04: empty.drop(1000000000) is empty") =
    empty.drop(1000000000).toList == Nil

  property("Ex03.05: The tail of constant list equals to the list") =
    given Arbitrary[Int] = Arbitrary { Gen.choose(1, 1000) }
    forAll  { (n: Int, m: Int) =>
      ones.drop(n).take(100).toList == ones.take(100).toList }
    
  // Exercise 4 (takeWhile)
 
  property("Ex04.00: takeWhile { _ < 10 } finds 9 naturals") = 
    naturals.takeWhile { _ < 10 }.toList.size == 9

  property("Ex04.01: naturals.takeWhile {_ < 10^9 }...") =
    naturals.takeWhile { _ < 1000000000 }.drop(100).take(50).toList.size == 50

  property("Ex04.02: takeWhile is consistent with take on naturals") =
    given Arbitrary[Int] = Arbitrary { Gen.choose(1, 1000) }
    forAll { (n: Int) =>
      naturals.takeWhile { _ < n }.toList == naturals.take(n - 1).toList }

  property("Ex04.03: takeWhile on 'to' finds at most one element") =
    forAll { (n: Int) =>
      to(n).takeWhile { _ % 2 == 0 }.toList.size <= 1 }

  property("Ex04.04: takeWhile is identity on empty") =
    forAll { (p: Int => Boolean) =>
      empty.takeWhile(p).toList == Nil }

  property("Ex04.05: takeWhile with false is empty") =
    forAll { (l: LazyList[Int]) =>
      l.takeWhile { _ => false }.toList == Nil }

  property("Ex04.06: takeWhile with false is empty (nontrivial)") =
    forAll { (n: Int) =>
      from(n).takeWhile { _ < n }.toList == Nil }

  // Exercise 5 (forAll)

  property("Ex05.00: The succesful test from the exercise text") = 
    ! naturals.forAll { _ < 0 }

  property("Ex05.01: forAll always true on empty") = 
    forAll { (p: Int => Boolean) => empty.forAll(p) }

  property("Ex05.02: forAll consistent with filter (requires Ex08 done)") =
    forAll { (p: String => Boolean) => 
      naturals
        .map { (_: Any).toString }
        .asInstanceOf[LazyList[String]]
        .filter(p)
        .map { (_: Any).toString }
        .take(100)
        .asInstanceOf[LazyList[String]]
        .forAll { (s: String) => p(s) } 
    }
    // messy maps and type casts above to allow compilation 
    // with very weak types (the question template)

  property("Ex05.03: forAll consistent with takeWhile") =
    forAll { (p: Int => Boolean) => 
      naturals.takeWhile(p).take(100).forAll(p) }

  property("Ex05.04: forAll implies exists on finite lists (requires Ex08 done)") =
    given Arbitrary[LazyList[Int]] = Arbitrary(nonEmptyLazyListOf[Int])
    forAll  { (p: Int => Boolean, l: LazyList[Int]) => 
      val l1 = l.filter(p)
      l1 != Empty  ==> l.exists(p) }

  // "Exercise 6 (takeWhile with foldRight)

  property("Ex06.00: takeWhile1 { _ < 10 } gives 9 naturals") = 
    naturals.takeWhile1 { _ < 10 }.toList.size == 9

  property("Ex06.01: takeWhile1 is like takeWhile") =
    forAll { (l: LazyList[Int], p: Int => Boolean) =>
      l.takeWhile(p).toList == l.takeWhile1(p).toList }
  
  // Exercise 7 (headOption with foldRight)

  property("Ex07.00: headOption1 (empty)") = 
    empty.headOption1 == None

  property("Ex07.01: headOption1 equivalent to headOption") =
    forAll { (l: LazyList[Int]) => l.headOption == l.headOption1 }

  property("Ex07.02: headOption1 works on an infinite list") =
    forAll { (n: Int) => to(n).headOption1 == Some(n) }

  // Exercise 8 (map, filter, append, flatMap)
  
  property("Ex08.00: map with identity is identity") =
    forAll { (l: LazyList[Int]) => l.map(identity).toList == l.toList }

  property("Ex08.01: map with a f and f^-1 is identity") =
    forAll { (l: LazyList[Int]) => 
      l.map { (_: Int) + 1 }.map { (_: Int) - 1 }.toList == l.toList }

  property("Ex08.02: map does not change size") =
    forAll { (l: LazyList[Int], f: Int => Boolean) => 
      l.map(f).toList.size == l.toList.size }

  property("Ex08.03: map on Empty is Empty") =
    forAll { (f: Int => String) => Empty.map(f) == Empty }

  property("Ex08.04: map is not forcing an infinite list") =
    naturals.map { (x: Any) => true }
    true

  property("Ex08.05: filter is not forcing an infinite list") =
    naturals.filter { (x: Any) => true }
    true

  property("Ex08.06: filter on Nil is Nil") =
    forAll { (f: Int => Boolean) => Empty.filter(f) == Empty }

  property("Ex08.07: filter with false is Nil") =
    forAll { (l: LazyList[Int]) => l.filter { (any: Any) => false } == Empty }
    // The peculiar type Any enables compiling a question template
    // with a weak type

  property("Ex08.08: filter with true is identity") =
    forAll { (l: LazyList[Int]) => 
      l.filter { (any: Any) => true }.toList == l.toList }
    // The peculiar type Any enables compiling a question template
    // with a weak type
  
  property("Ex08.09: Empty is left zero for append") = 
    forAll { (l: LazyList[Int]) => Empty.append(l).toList == l.toList }

  property("Ex08.10: Empty is right zero for append") = 
    forAll { (l: LazyList[Int]) => l.append(Empty).toList == l.toList }

  property("Ex08.11: append is associative") = 
    forAll { (l: LazyList[Int], m: LazyList[Int], n: LazyList[Int]) => 
      l.append(m).append(n).toList == l.append(m.append(n)).toList }

  property("Ex08.12: append does not force an infinite list") =
    naturals.append(naturals)
    true

  property("Ex08.13: flatMap on Empty is Empty") =
    forAll { (f: Int => LazyList[Int]) => Empty.flatMap(f) == Empty }

  property("Ex08.14: flatMap is associative") =
    forAll { (l: LazyList[Int], 
      f: Int => LazyList[Int], g: Int => LazyList[Int]) =>
      val left = l.flatMap(f).flatMap(g)
      val right = l.flatMap { (n: Int) => f(n).flatMap(g) }
      left.toList == right.toList 
    }
    // Nasty type cast on n to allow compiling the broken type in the
    // question template
    
  property("Ex08.15: map is flatMap with unit (Cons)") =
    forAll { (l: LazyList[Int], f: Int => Int) => 
      l.map(f).toList == l.flatMap { (n: Int) => cons(f(n), empty) }.toList }

  property("Ex08.16: flatMap does not force an infinite list") =
    naturals.flatMap { (n: Any) => naturals }
    true

  // Exercise 10 (fibs)
  property("Ex10.00: fib.head is zero") =
    fibs.asInstanceOf[LazyList[Int]].headOption == Some(0)

  property("Ex10.01: fib.tail.head is one") =
    fibs.asInstanceOf[LazyList[Int]].tail.headOption == Some(1)

  property("Ex10.02: check the Fibonacci invariant") =
    forAll (Gen.choose(0, 1000)) { (offset: Int) =>
      val List(i, j, k) = fibs.asInstanceOf[LazyList[Int]]
        .drop(offset)
        .take(3)
        .toList
      k == i + j 
    }

  // Exercise 11 (unfold)
 
  property("Ex11.00: Unfold with immediate failure gives empty") = 
    unfold(()) { _ => None } == Empty

  private def gen(m: Int)(n: Int) = 
    if n > m then None else Some(n,  n + 1)

  property("Ex11.02: Unfold 1 element") =
    unfold(0)(gen(0)).toList == List(0) 

  property("Ex11.03: Unfold m elements") =
    forAll (Gen.choose(0, 1000)) { (m: Int) =>
      unfold(0)(gen(m)).toList.size == m + 1 }

  // Exercise 12 (fibsUnfold)
  
  property("Ex12.00: fibs and fibsUnfold are equivalent") =
    forAll (Gen.choose(0, 1000)) { k =>
      val left = fibs.asInstanceOf[LazyList[Int]].drop(k)
      val right = fibsUnfold.asInstanceOf[LazyList[Int]].drop(k)
      left.headOption == right.headOption 
    }

  // Exercise 13 (mapUnfold, takeUnfold, takeWhileUnfold, zipWith)

  property("Ex13.00: mapUnfold equivalent to map") =
    forAll { (l: LazyList[Int], f: Int => Int) =>
      l.map(f).toList == l.mapUnfold(f).toList }

  property("Ex13.01: mapUnfold does not force") =
    naturals.mapUnfold { (n: Any) => naturals }
    true

  property("Ex13.02: takeUnfold equivalent to take") =
    forAll { (l: LazyList[Int], n: Int) =>
      l.take(n).toList == l.takeUnfold(n).toList }

  property("Ex13.03: takeUnfold does not force infinitely many elements") =
    naturals.takeUnfold(100) 
    true

  property("Ex13.04: takeWhileUnfold equivalent to takeWhile") =
    forAll { (l: LazyList[Int], p: Int => Boolean) =>
      l.takeWhile(p).toList == l.takeWhileUnfold(p).toList }

  property("Ex13.05: mapUnfold does not force infinitely many elements") =
    naturals.mapUnfold { x => x + 1 } 
    true

  private def cbn[A, B](p: (A, A) => B)(x: => A, y: => A): B = p(x,y)
  property("Ex13.06: zipWith Empty is Empty") =
    forAll { (l: LazyList[Int], p: (Int, Int) => Int) =>
      Empty.zipWith[Int, Int](cbn(p))(l) == Empty }

  property("Ex13.07: zipWith does not force") =
    forAll { (l: LazyList[Int], p: (Int, Int) => Int) =>
      naturals.zipWith[Int, Int](cbn(p))(naturals)
      true }

  property("Ex13.07: zipWith behaves like on lists") =
    forAll { (l: LazyList[Int], m: LazyList[Int]) =>
      val left = l.zipWith { cbn((_,_)) } (m).toList
      val right = l.toList.zip(m.toList)
      left == right }
