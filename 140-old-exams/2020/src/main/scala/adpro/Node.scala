package adpro

sealed trait Node[+A] {

  import Node._

  def toList: List[A] = reduceNode.toList[A] (this)
}

case class Node2[A] (l: A, r: A) extends Node[A]
case class Node3[A] (l: A, m: A, r: A) extends Node[A]

object Node {

  implicit val reduceNode = new Reduce[Node] {

    def reduceR[A,Z] (op: (A,Z) => Z) (n: Node[A], z: Z): Z =
      n match {

        case Node2 (l,r) =>
          op (l, op (r, z))

        case Node3 (l,m,r) =>
          op (l, op (m, op (r, z)))
      }


    def reduceL[A,Z] (op: (Z,A) => Z) (z: Z, n: Node[A]): Z = n
      match {

        case Node2 (l,r) =>
          op (op (z, l), r)

        case Node3 (l,m,r) =>
          op (op (op (z, l), m), r)
      }
  }

}

