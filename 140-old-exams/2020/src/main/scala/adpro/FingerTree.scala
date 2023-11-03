package adpro

import Digit._
import Reduce.reduce

sealed trait FingerTree[+A] {

  def ▷ [B >: A] (b: B): FingerTree[B] =
    FingerTree.addR (this, b)

  def addL[B >: A] (b: B): FingerTree[B] =
    FingerTree.addL (b, this)

  def addR[B >: A] (b: B): FingerTree[B] =
    FingerTree.addR (this, b)

  def toList: List[A] =
    FingerTree.reduceTree.toList (this)

  def headL: A =
    FingerTree.headL (this)

  def tailL: FingerTree[A] =
    FingerTree.tailL (this)

  def headR: A =
    FingerTree.headR (this)

  def tailR: FingerTree[A] =
    FingerTree.tailR (this)

  // A quick size
  def size: Int = this.toList.size

  def empty = false
  def nonEmpty = true

}

// Renamed from Empty, to avoid name conflict with Stream
case class EmptyDeque () extends FingerTree[Nothing] {

  override def empty = true
  override def nonEmpty = false

}

case class Single[A] (data: A) extends FingerTree[A]

case class Deep[A] (

  pr: Digit[A],
  m: FingerTree[Node[A]],
  sf: Digit[A]

) extends FingerTree[A]


object FingerTree {

  import Reduce._

  implicit def reduceTree: Reduce[FingerTree] = new Reduce[FingerTree] {

    def reduceR[A,Z] (opr: (A,Z) => Z) (t: FingerTree[A], z: Z): Z =
      t match {

        case EmptyDeque () =>
          z

        case Single (x) =>
          opr (x, z)

        case Deep (pr, m, sf) =>
          val o1 = reduce[Digit].reduceR (opr) _
          val o2 = this.reduceR (reduce[Node].reduceR (opr)) _
          o1 (pr, o2 (m, o1 (sf,z)))
      }


    def reduceL[A,Z] (opl: (Z,A) => Z) (z: Z, t: FingerTree[A]): Z =
      t match {

        case EmptyDeque () =>
          z

        case Single (x) =>
          opl (z,x)

        case Deep (pr, m, sf) =>
          val o1 = reduce[Digit].reduceL (opl) _
          val o2 = reduceL (reduce[Node].reduceL (opl)) _
          o1 (o2 (o1 (z, pr), m), sf)
      }
  }

  implicit class LeftFingerTreeOps[A]  (a: A) {
    def ◁ [B >: A] (t: FingerTree[B]): FingerTree[B] =
      addL[B] (a,t)
  }

  def addL[A] (a: A, t: FingerTree[A]): FingerTree[A] =
    t match {
      case EmptyDeque () =>
        Single (a)

      case Single (b) =>
        Deep (Digit (a), EmptyDeque (), Digit (b))
      case Deep (Digit (b,c,d,e), m, sf) =>
        Deep (Digit(a,b), addL[Node[A]] (Node3 (c,d,e), m), sf)
        // Deep (Digit(a,b), Node3 (c,d,e) ◁  m, sf)

      case Deep (pr, m, sf) =>
        Deep (a::pr, m, sf)
    }


  def addR[A] (t: FingerTree[A], a: A): FingerTree[A] =
    t match {
      case EmptyDeque () =>
        Single (a)

      case Single(b) =>
        Deep (Digit (b), EmptyDeque (), Digit (a))

      case Deep (pr, m, Digit(e,d,c,b)) =>
        Deep (pr, addR[Node[A]] (m, Node3 (e,d,c)), Digit (b,a))

      case Deep (pr, m, sf) =>
        Deep (pr, m, sf ++ List(a))
  }

  def toTree[F[_]: Reduce, A] (fa:  F[A]): FingerTree[A] =
    reduce[F].reduceR[A, FingerTree[A]] (addL) (fa, EmptyDeque ())

  def deepL[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A])
    : FingerTree[A] =
      pr match {

        case scala.collection.immutable.Nil =>
          m match {
            case NilDeque () =>
              FingerTree.toTree (sf)
            case ConsL (a,m1) =>
              Deep (a.toList, m1, sf)
          }

        case pr =>
          Deep (pr, m, sf)
      }

  def deepR[A] (pr: Digit[A], m: FingerTree[Node[A]], sf: Digit[A]): FingerTree[A] =
    sf match {

      case scala.collection.immutable.Nil =>
        m match {
          case NilDeque () =>
            FingerTree.toTree (pr)

          case ConsR (m1,a) =>
            Deep (pr, m1, a.toList)
        }

      case sf =>
        Deep (pr, m, sf)
    }

  def headL[A] (t: FingerTree[A]): A =
    t match {
      case ConsL(h,_) => h
    }

  def tailL[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsL(_,t) => t
    }

  def headR[A] (t: FingerTree[A]): A =
    t match {
      case ConsR(_,h) => h
    }

  def tailR[A] (t: FingerTree[A]): FingerTree[A] =
    t match {
      case ConsR(t,_) => t
    }

}


object NilDeque {
  def unapply[A] (t: FingerTree[A]): Boolean =
    t match {
      case EmptyDeque () => true
      case _ => false
    }
}

object ConsL {
  def unapply[A] (t: FingerTree[A]) :Option[(A, FingerTree[A])] =
    t match {

      case EmptyDeque () =>
        None

      case Single (x) =>
        Some (x, EmptyDeque ())

      case Deep (pr,m,sf) =>
        Some (pr.head, FingerTree.deepL (pr.tail, m, sf))
    }
}

object ConsR {

  def unapply[A] (t: FingerTree[A]) :Option[(FingerTree[A],A)] =
    t match {

      case EmptyDeque () =>
        None

      case Single (x) =>
        Some (EmptyDeque (), x)

      case Deep (pr, m, sf) =>
        val sfr = sf.reverse
        Some (FingerTree.deepR[A] (pr, m, sfr.tail.reverse), sfr.head)
    }
}


