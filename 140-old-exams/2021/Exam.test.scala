/**
 * This file is empty on purpose.   It is added, and configured if you
 * wanted to experiment with tests.
 */

package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

object Exam2021AutumnSpec
  extends org.scalacheck.Properties("exam-2021"):

  // Q1

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }
