// Advanced Programming, Wasowski, IT University of Copenhagen
package adpro

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._

import adpro.Monoid.monoid

trait Reduce[F[_]] { self =>

  def reduceR[A, B] (opr: (A,B) => B) (fa: F[A], b: B): B

  def reduceL[A, B] (opl: (B,A) => B) (b: B, fa: F[A]): B

  def toList[A] (fa: F[A]): List[A] =
    reduceR[A, List[A]] (_::_) (fa, scala.collection.immutable.Nil)

  object Laws {

    def reducersEquivalentForMonoids[A: Monoid]
      (implicit arbFA: Arbitrary[F[A]]) =
        forAll { fa: F[A] =>
          val M = monoid[A]
          self.reduceR[A,A] (M.op _) (fa, M.zero)  should
            be (self.reduceL[A,A] (M.op _) (M.zero , fa))
        }
  }

}

object Reduce {

  def reduce[F[_]: Reduce]: Reduce[F] =
    implicitly[Reduce[F]]

  implicit val reduceList = new Reduce[List] {

    def reduceR[A, Z] (opr: (A, Z) => Z) (l: List[A], z: Z): Z =
      l.foldRight (z) (opr)

    def reduceL[A, Z] (opl: (Z, A) => Z) (z: Z, l: List[A]): Z =
      l.foldLeft  (z) (opl)

  }

}
