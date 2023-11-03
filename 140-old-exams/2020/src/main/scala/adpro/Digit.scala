package adpro

import Reduce.reduce

object Digit {

  type Digit[A] = List[A]

  def apply[A] (as: A*) : Digit[A] = List(as:_*)

  def unapplySeq[A] (d: Digit[A]): Option[Seq[A]] =
    Some (d)
}


