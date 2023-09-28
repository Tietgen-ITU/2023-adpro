// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.lazyList

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  import LazyList.*

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h,t) => Some(h())

  def tail: LazyList[A] = this match
    case Empty => Empty
    case Cons(h,t) => t()

  def toList: List[A] = this match
    case Empty => Nil
    case Cons(h, t) => h():: t().toList
 
  def take(n: Int): LazyList[A] = this match
    case Empty => 
      Empty
    case Cons (h, t) => 
      if n == 0 
      then Empty 
      else Cons(h, () => t().take(n - 1))

  def drop(n: Int): LazyList[A] = this match
    case Empty => 
      Empty
    case Cons(_, t) => 
      if n == 0 
      then this 
      else t().drop(n - 1)

object LazyList:

  def empty[A]: LazyList[A] = Empty

  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty 
    then empty
    else cons(as.head, apply(as.tail*))
