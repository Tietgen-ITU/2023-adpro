// Advanced Programming, A. Wąsowski, IT University of Copenhagen 

package adpro.lens

import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalacheck.Arbitrary.*
import org.scalacheck.Prop.*

import monocle.*
import monocle.syntax.applied.*
import monocle.{Lens, Optional}

object LensesSpec
  extends org.scalacheck.Properties("lens.."):

  // Exercise 1 (L1, L2, L3)

  property("Ex01.00: No tests now. We test this in Exercise 3") = 
    true

  // Exercise 2 (laws)

  property("Ex02.00: No tests now. We test this in Exercise 3") = 
    true

  // Exercise 3 (tests)

  // TODO: add macro tests that inspect that tests in Ex03 are correct, one day
  // TODO: perhaps also add the ones with failing properties for L2 and L3 
  // (I think in some earlier classes there was even some code to try failing
  // tests)

  // Exercise 4 (codiag)

  property("Ex04.00: codiag[Int] and codiag[String] are very well behaved") =
    (Laws.veryWellBehavedTotalLense(codiag[Int]) :| "codiag[Int]") 
      && (Laws.veryWellBehavedTotalLense(codiag[String]) :| "codiag[String]")

  // Exercise 5 (codiag1)
  // TODO: add a macro test that inspects the solution of codiag1, one day

  property("Ex05.00: codiag1[Int] and codiag1[String] are very well behaved") = 
    Laws.veryWellBehavedTotalLense(codiag1[Int])
      && Laws.veryWellBehavedTotalLense(codiag1[String])

  // Exercise 6 (itu1, moving Alex to 9100")

  property("Ex06.01: itu should show Alex at zipcode 2800") =
    itu.students ("Alex").zipcode == "2800"

  property("Ex06.02: itu1 should show Alex at zipcode 9100") =
    itu1.students ("Alex").zipcode == "9100"

  property("Ex06.03: itu1 should be the same as itu for others than Alex") =
    forAll(Gen.oneOf(itu.students.keySet)) { (name: Name) =>
      name != "Alex" ==> 
        (itu1.students(name).zipcode == itu.students(name).zipcode) }

  // Exercise 7 (lenses for the university)

  given Arbitrary[Address] = Arbitrary(Gen.resultOf(Address.apply))
  given Arbitrary[University] = Arbitrary(Gen.resultOf(University.apply))
  
  property("Ex07.00: _zipcode is total and very well behaved") =
    Laws.veryWellBehavedTotalLense(_zipcode)

  property("Ex07.01: _students is very well behaved") =
    Laws.veryWellBehavedTotalLense(_students)

  property("Ex07.02: itu2 should show Alex at zipcode 9100") =
    itu2.students("Alex").zipcode == "9100"

  // Exercise 8 (lenses for field accesses)

  property("Ex08.00: _zipcode1 is total and very well behaved") =
    Laws.veryWellBehavedTotalLense(_zipcode1)

  property("Ex08.02: _students1 is very well behaved") =
    Laws.veryWellBehavedTotalLense(_students1)

  property("Ex08.03: itu3 should show Alex at zipcode 9100") =
    itu3.students("Alex").zipcode == "9100"

  property("Ex08.04: itu4 should show Alex at zipcode 9100") =
    itu4.students("Alex").zipcode == "9100"

  // Exercise 9 (capitalizing countries)

  property("Ex09.00: itu5 should have all the countries in upper case") =
    itu5
     .students
     .values
     .map { _.country }
     .forall { s => s.toUpperCase == s }

  // Exercise 10 (capitalizing selected countries)

  property("Ex10.00: itu6 should have selected countries in upper case") =
    itu6
     .students
     .filter { (s, a) => s(0) == 'A' }
     .values
     .map { _.country }
     .forall { s => s.toUpperCase == s }

  property("Ex10.01: The remaining countries in itu6 are unchanged") =
    itu6
     .students
     .filter { (s, a) => s(0) != 'A' }
     .keys
     .forall { s => itu6.students.get(s) == itu.students.get(s) }

  // Exercise 11 (ith)

  property("Ex11.00: partial get success (ith)") =
    forAll { (l: List[Int]) =>
      l.nonEmpty ==> {
        forAll(Gen.choose(0, l.size-1)) { (n: Int) =>
          ith[Int](n).getOption(l) == Some(l(n)) }
    } }

  property("Ex11.01: partial get failure (ith)") =
    forAll { (l: List[Int]) =>
      l.nonEmpty ==> {
        forAll(Gen.choose(l.size, l.size + 100)) { (n: Int) =>
          ith[Int](n).getOption(l) == None  
    } } }

  property("Ex11.02: partial replace success (ith)") =
    forAll { (l: List[Int]) =>
      l.nonEmpty ==> {
        forAll(Gen.choose(0, l.size-1)) { (n: Int) =>
          ith[Int](n).replace(42)(l) == l.updated(n, 42)
    } } }

  property("Ex11.03: partial replace failure (ith)") =
    forAll { (l: List[Int]) =>
      l.nonEmpty ==> {
       forAll(Gen.choose(l.size, l.size + 100)) { (n: Int) =>
           ith[Int](n).replace(42)(l) == l 
    } } }

  property("Ex11.04: ith1 is well behaved (total)") =
   forAll (Gen.choose (0, 10000)) { (n: Int) =>
     ith1[Int](42)(n+3).get(List(1,2,3)) == 42
       && ith1[Int](42)(n).replace(42)(Nil) == List.fill(n+1)(42) }

  // Exercise 12 (update list)

  property("Ex12.00: update a list using 'ith' List (1..6)(3)++") =
    list1 == List (1,2,4,4,5,6)
