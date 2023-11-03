package adpro

import org.scalacheck.Arbitrary
import org.scalatest.{FreeSpec,Matchers}
import org.scalatest.prop._
import Arbitrary.arbitrary

class TrivialSpec extends FreeSpec with PropertyChecks with Matchers {

  "Trivial Monad is a monad" - {

    import Exam2019AutumnSolution.MonadsAndTesting._

    val TM = Trivial

    implicit def arbTrivial[A: Arbitrary]: Arbitrary[Trivial[A]] =
      Arbitrary {
        for { a <- implicitly[Arbitrary[A]].arbitrary }
        yield Trivial.unit (a)
      }

    "Trivial monad satisfies the identity law" in {

      forAll { (tmx: Trivial[Int]) => Trivial.map (tmx) (identity) shouldBe tmx }

    }

    "Trivial monad is associative" in {

      forAll { (tmx: Trivial[Int],
        f: Int => Trivial[Int],
        g: Int => Trivial[Int] ) =>
          TM.flatMap[Int,Int] (TM.flatMap (tmx) (f)) (g) shouldBe
            TM.flatMap (tmx) (x => TM.flatMap (f(x)) (g))
      }
    }

  }

}
