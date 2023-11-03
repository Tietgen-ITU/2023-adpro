package adpro

import org.scalatest.{FreeSpec,Matchers,Inside,OptionValues}
import org.scalatest.prop._
import org.scalacheck.Arbitrary
import org.scalatest.exceptions.TestFailedException

import adpro.data._

class Exam2019AutumnSolutionSpec extends FreeSpec
  with Inside
  with Matchers
  with PropertyChecks
  with OptionValues {

  import Exam2019AutumnSolution._

  "Task Lenses" - {

    import monocle.{Lens, Optional}
    import Lenses._

    implicit def fingerTree[A: Arbitrary]: Arbitrary[FingerTree[A]] =
      Arbitrary {
        implicitly[Arbitrary[List[A]]]
          .arbitrary
          .map (Digit.toTree[A])
      }

    // Laws (Specialized for Finger Trees to admit comparisons of
    // linearlizations to list)

    def PutGetOptional[C: Arbitrary, A: Arbitrary] (l: Optional[C,A]) =
      forAll { (c: C, a: A) =>
        l.getOption (l.set (a) (c)).value shouldBe a }

    def GetPutOptional[A: Arbitrary] (l: Optional[FingerTree[A],A]) =
      forAll { c: FingerTree[A] =>
        inside (l getOption c) {
          case Some (a) => {l.set (a) (c)}.toList shouldBe c.toList
          case None => assert (true)
        }
      }

     def PutPutOptional [A: Arbitrary] (l: Optional[FingerTree[A],A]) =
       forAll { (a:A, a1: A, c: FingerTree[A]) =>
         l.set (a1) (l.set (a) (c)).toList shouldBe l.set (a1) (c).toList }

    // Tests

    // peekR

    "peekR put-get" in { PutGetOptional (peekR[Int]) }
    "peekR get-put" in { GetPutOptional (peekR[Int]) }
    "peekR put-put" in { PutPutOptional (peekR[Int]) }

    // peekRR

    "peekRR put-get" in { PutGetOptional (peekRR[Int]) }
    "peekRR get-put" in { GetPutOptional (peekRR[Int]) }

    "peekRR put-put" in {
      withClue ("This test fails intermittently, for probabilistic reasons?") {
        intercept[TestFailedException]{ PutPutOptional (peekRR[Int]) }
      }
    }

    // peekLL

    "peekLL put-get" in { PutGetOptional (peekLL[Int]) }
    "peekLL get-put" in { GetPutOptional (peekLL[Int]) }

    "peekLL put-put" in {
      withClue ("This test fails intermittently, for probabilistic reasons?") {
        intercept[TestFailedException]{ PutPutOptional (peekLL[Int]) }
      }
    }

  }

}
