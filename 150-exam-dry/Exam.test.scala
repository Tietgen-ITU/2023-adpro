/* This file is empty on purpose.   It is added, and configured if you
 * wanted to add your own tests during the exam.  It is not graded and
 * should not be submitted.
 */
package adpro

import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import adpro.laziness.*

object ExamSpec
  extends org.scalacheck.Properties("exam-2023-autumn"):

  def toAdproLazyList[A](xs: List[A]): LazyList[A] =
    xs.foldRight(LazyList.empty)((x, acc) => LazyList.cons(x, acc))

  property("A test that always passes (a sanity check)") = 
    forAll { (n: Int) => n == n }

  property("Q1: Test that they give the same answer") =
    forAll((xs: List[Int]) => Streaming.fViaRec(toAdproLazyList(xs)) == Streaming.fViaFold(toAdproLazyList(xs)))

end ExamSpec

object NullUpdatesSpecObj
  extends RL.NullUpdatesSpec(update = RL.update, "studentrl") {}
