/**
 * This file is empty on purpose.   It is added, and configured if you
 * wanted to experiment with tests.
 */

//> using scala "3.1.3"
//> using lib "org.scalacheck::scalacheck:1.16.0"
//> using lib "org.scalactic::scalactic:3.2.14"
//> using lib "dev.optics::monocle-core:3.1.0"
//> using lib "dev.optics::monocle-macro:3.1.0"
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

object Exam2021AutumnSpec
  extends org.scalacheck.Properties("exam-2021"):

  // Q1

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }
