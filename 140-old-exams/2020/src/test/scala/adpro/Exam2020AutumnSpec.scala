/**
 * This file is empty on purpose.   It is added, and configured if you
 * wanted to experiment with tests.
 */
package adpro

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

class Exam2020AutumnSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "Q1" - {

    import Q1._

    "A test that always passes (a sanity check)" in {
      forAll { (n: Int) => n shouldBe n }
    }

    //  You can add tests for Q1 here, etc...

  }

}
