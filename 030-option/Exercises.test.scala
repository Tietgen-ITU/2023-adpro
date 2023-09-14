// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*

import Tree.* 
import Option.* // Use our option, not the standard library

// deactivate shrinking
given Shrink[Int] = Shrink.shrinkAny

def genTree[A](using arbA: Arbitrary[A]): Gen[Tree[A]] =
  for coin <- Gen.frequency(7 -> true, 1 -> false)
      tree <- if coin 
        then arbA.arbitrary.map (Leaf.apply)
        else genTree.flatMap { l => genTree.map { Branch(l, _) } }
  yield tree

given arbitraryTree[A: Arbitrary]: Arbitrary[Tree[A]] =
  Arbitrary[Tree[A]] (genTree)

given arbitraryOption[A](using arbA: Arbitrary[A]): Arbitrary[Option[A]] =
  Arbitrary { Gen.oneOf(Gen.const(None), arbA.arbitrary.map(Some.apply)) }

def nonEmptyIntList(using Arbitrary[Int]): Gen[List[Int]] = for
  n <- Gen.choose(1, 500)
  l <- Gen.listOfN[Int](n, summon[Arbitrary[Int]].arbitrary)
yield l


// Generator for Exercise 1

import java.awt.Point

given arbitraryPoint(using Arbitrary[Int]): Arbitrary[Point & OrderedPoint] =
  Arbitrary { for
    x <- summon[Arbitrary[Int]].arbitrary
    y <- summon[Arbitrary[Int]].arbitrary
  yield new Point (x,y) with OrderedPoint }


object ExercisesSpec
  extends org.scalacheck.Properties("option"):

  // Exercise 1 (OrderedPoint)
 
  property("Ex01.00: The scenario test from the exercise text") =
    val p = new Point(0, 1) with OrderedPoint
    val q = new Point(0, 2) with OrderedPoint
    p < q
    
  property("Ex01.01: A non-negative shift preserves order") =
    val coord = Gen.choose(-10000, +10000)
    val delta = Gen.choose(0, 10000)
    forAll (coord, coord, delta, delta) { (x, y, a, b) =>
      val p = new Point(x, y) with OrderedPoint
      val q = new Point(x + a, y + b) with OrderedPoint
      (  p <= q ) :| "  p <= q" && 
      (!(p >  q)) :| "  !(p > q)"
    }

  property("Ex01.02: (<) is a total order") = forAll {
    (x: Point & OrderedPoint, y: Point & OrderedPoint) =>
    { (x < y) ==> !(y < x)       } :| "  < is not >" && 
    { (x < y) ==> (y > x)        } :| "  < not symmetric" &&
    { (x < y || y < x || x == y) } :| "  order should be total" &&
    { (x <= x) } :| "  ≤ should be reflexive" &&
    { (x >= x) } :| "  ≥ should be reflexive"
  }

  // Exercise 2 (size)

  property("Ex02.00: A simple scenario test") = 
    Tree.size(Branch(Leaf(1), Leaf(2))) == 3

  property("Ex02.01: A leaf should be size 1") =
    forAll { (n: Int) => Tree.size(Leaf(n)) == 1 }

  // Exercise 3 (Tree maximum)

  property("Ex03.00: a simple scenario test") =
    Tree.maximum(Branch(Leaf(1), Leaf(2))) == 2

  property("Ex03.01: a singleton tree test") =
    forAll { (n: Int) => Tree.maximum(Leaf(n)) == n }

  property("Ex03.02: a bi-node tree test") =
    forAll { (n: Int, m: Int) => 
      Tree.maximum(Branch(Leaf(m), Leaf(n))) == n.max (m) }

  property("Ex03.03: a tri-node tree test") =
    forAll { (n: Int, m: Int, o: Int) =>
      val M = n max m max o
      Tree.maximum(Branch(Leaf(m), Branch(Leaf(n), Leaf(o)))) == M
      Tree.maximum(Branch(Leaf(m), Branch(Leaf(n), Leaf(o)))) == M
    }

  property("Ex03.03: Max in a tree with all the same label is the label") =
    given Arbitrary[Int] = Arbitrary[Int] { Gen.const[Int] (42) }
    forAll { (t: Tree[Int]) =>  Tree.maximum(t) == 42 }

  // Exercise 4 (map)

  val t4 = Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf (4)))
  val t5 = Branch(Leaf("1"), Branch(Branch(Leaf("2"), Leaf("3")), Leaf ("4")))

  property("Ex04.00: a simple cross-type scenario test") =
    Tree.map(t4)(_.toString) == t5

  property("Ex04.01: a simple identity scenario test") =
    Tree.map(t4)(identity[Int]) == t4

  property("Ex04.02: identity is a unit with map") =
    forAll { (t: Tree[Int]) => Tree.map(t)(identity[Int]) == t }

  property("Ex04.03: map does not change size") =
    forAll { (t: Tree[Int], f: Int => Int) =>
      Tree.size(Tree.map(t)(f)) == Tree.size(t) }

  property("Ex04.04: map is 'associative'") = 
    forAll { (t: Tree[Int], f: Int => Int, g: Int => Int) =>
      Tree.map (Tree.map(t)(f))(g) == Tree.map(t)(g compose f) }

  // Exercise 5 (fold)
  
  property("Ex05.00: fold over a leaf equivalent to applying the operator") =
    forAll { (f: (Int, Int) => Int, g: Int => Int, n: Int) =>
      fold(Leaf(n))(f)(g) == g(n) }

  property("Ex05.01: fold over a branch is the operator on leafs") =
    forAll { (f: (Int, Int) => Int, l: Int, r: Int) =>
      fold(Branch(Leaf(l), Leaf(r)))(f)(identity[Int]) == f(l,r) }

  property("Ex05.02: size1 behaves like size (check that you use fold!)") =
    forAll { (t: Tree[Int]) => Tree.size(t) == Tree.size1(t) }

  property("Ex05.03: maximum1 behaves like size (check that you used fold!)") =
    forAll { (t: Tree[Int]) => Tree.maximum(t) == Tree.maximum1(t) }

  property("Ex05.04: map1 behaves like map (check that you used fold!)") =
    forAll { (t: Tree[Int], f: Int => Int) =>
      Tree.map(t)(f) == Tree.map1(t)(f) }

  // Exercise 6 (Option basics)

  property("Ex06.00: Option map does nothing on None") = 
    forAll { (f: Int => Int) => None.map(f) == None }

  property("Ex06.01: Option map on Some just maps the element") = 
    forAll { (n: Int, f: Int => Int) => Some(n).map(f) == Some(f(n)) }

  property("Ex06.02: Option getOrElse on None always does else") = 
    forAll { (m: Int) => None.getOrElse(m) == m }

  property("Ex06.03: Option getOrElse on Some(n) always gives n") = 
    forAll { (n: Int, m: Int) => Some(n).getOrElse(m) == n }

  property("Ex06.04: Option flatMap on None does None") = 
    forAll { (f: Int => Option[Int]) => None.flatMap(f) == None }

  property("Ex06.05: Option flatMap to None gives None") = 
    forAll { (o: Option[Int]) => o.flatMap { _ => None } == None }

  property("Ex06.06: Option flatMap on Some applies the function or fails") = 
    forAll { (n: Int, f: Int => Option[Int]) => Some(n).flatMap(f) == f(n) }

  property("Ex06.07: Option filter on None gives None") = 
    forAll { (p: Int => Boolean) => None.filter(p) == None }

  property("Ex06.08: Option filter with constant false predicate gives None") = 
    forAll { (o: Option[Int]) => o.filter { _ => false } == None }

  property("Ex06.09: Option filter with constant true predicate is identity") = 
    forAll { (o: Option[Int]) => o.filter { _ => true } == o }

  property("Ex06.10: Option filter picks the value iff it satisfies the predicate") = 
    forAll { (n: Int, p: Int => Boolean) =>
      Some(n).filter(p) == (if p(n) then Some(n) else None) }

  // Exercise 7 (headGrade)

  property("Ex07.00: headAge of empty list == None") = 
    headGrade(Nil) == None

  property("Ex07.01: the value from the first pair correctly extracted") =
    forAll { (tail: List[(String,Int)], name: String, grade: Int) =>
      headGrade((name, grade):: tail) == Some(grade) }
  
  // Exercise 8 (variance)

  property("Ex08.00: simple fixed scenario 1") = 
    variance(List(42,42,42)) == Some(0.0)

  property("Ex08.01: simple fixed scenario 2") = 
    variance(Nil) == None

  property("Ex08.02: Variance of a singleton is always zero") =
    // Very large numbers lead to overflows so limit
    given Gen[Double] = Gen.choose[Double](-10000.0, 10000.0)
    forAll { (x: Double) => variance(List(x)) == Some(0.0) }

  property("Ex08.03: A constant list x,x,x,x,... has variance zero") = 
    given Arbitrary[Int] = Arbitrary(Gen.choose(2, 30))
    given Arbitrary[Double] = Arbitrary(Gen.choose(-100.0, 100.0))
    forAll { (x: Double, n: Int) =>
      variance(List.fill(n)(x))
        .forAll { _ <= 0.001 } 
    }

  property("Ex08.04: -x, x, -x, x, ... has variance x*x") = 
    given Arbitrary[Int] = Arbitrary(Gen.choose(1, 100))
    given Arbitrary[Double] = Arbitrary(Gen.choose(.001, 10000.0))
    forAll { (x: Double, n: Int) =>
        val l = List.fill(n)(List(-x, x)).flatMap(identity)
        variance(l) 
          .forAll { v => Math.abs(v - x*x) <= 0.0001 }
    }

  // Exercise 9 (map2)

  property("Ex09.00: Some + None gives None") = 
    forAll { (o: Option[Int], f: (Int,Int) => Int) =>
      map2(o, None) (f) == None }

  property("Ex09.01: None + Some gives None") = 
    forAll { (o: Option[Int], f: (Int,Int) => Int) =>
      map2(None, o)(f) == None }

  property("Ex09.02: None + None gives None") = 
    forAll { (f: (Int,Int) => Int) => map2(None, None)(f) == None }

  property("Ex09.03: two Some elements are nicely merged by subtraction") = 
    forAll { (n: Int, m: Int) => map2(Some(n), Some(m))(_-_) == Some(n - m) }

  property("Ex09.04: two Some elements are nicely merged by arbitrary f") = 
    forAll { (n: Int, m: Int, f: (Int,Int) => Int) =>
        map2(Some(n), Some(m))(f) == Some(f(n, m)) }

  // Exercise 10 (sequence)

  property("Ex10.00: some simple scenarios #1") = 
    sequence(Nil) == Some(Nil)

  property("Ex10.01: some simple scenarios #2") = 
    sequence(List(None)) == None

  property("Ex10.02: some simple scenarios #3") = 
    sequence(List(Some(42))) == Some(List(42))

  property("Ex10.03: some simple scenarios #4") = 
    sequence(List(Some(1), Some(2), Some(42))) == Some(List(1, 2, 42))

  property("Ex10.04: some simple scenarios #5") = 
    sequence(List(None, Some(2), Some(42))) == None

  property("Ex10.05: some simple scenarios #6") = 
    sequence(List(Some(1), None, Some(42))) == None

  property("Ex10.06: some simple scenarios #7") = 
    sequence(List(Some(1), Some(2), None)) == None

  property("Ex10.07: A list without Nones sequences to itself") =
    forAll { (l: List[Int]) => 
      sequence(l.map(Some.apply)) == Some(l) }

  property("Ex10.08: A list with None sequences to None") = 
    forAll { (l: List[Option[Int]]) =>
      sequence(l ++ (None ::l)) == None }

  // Exercise 11 (traverse)

  property("Ex11.00: some simple scenarios #1") = 
    traverse(List(1, 2, 42))(Some.apply) == Some(List(1, 2, 42))

  property("Ex11.01: some simple scenarios #2") = 
    def f(n: Int): Option[Int] =
      Some(n).filter { _ % 2 == 0 }
    traverse(List(1, 2, 42)) (f) == None

  property("Ex11.02: traversal of bottom gives bottom") = 
    given Arbitrary[List[Int]] = Arbitrary(nonEmptyIntList)
    forAll { (l: List[Int]) => traverse(l) { _ => None } == None }

  property("Ex11.03: empty traversal cannot fail") =
    forAll { (f: Int => Option[Int]) => traverse(Nil)(f) == Some(Nil) }
