// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.option

// Exercise 1

trait OrderedPoint 
  extends scala.math.Ordered[java.awt.Point]:

  this: java.awt.Point =>

  override def compare(that: java.awt.Point): Int = 
    if this.x > that.x then 1
    else if this.x < that.x then -1
    else if this.y > that.y then 1
    else if this.y < that.y then -1
    else 0

// Try the following (and similar) tests in the repl (sbt console):
//
// import adpro._
// val p = new java.awt.Point(0, 1) with OrderedPoint
// val q = new java.awt.Point(0, 2) with OrderedPoint
// assert(p < q)



// Chapter 3 Exercises

enum Tree[+A]:
  case Leaf(value: A)
  case Branch(left: Tree[A], right: Tree[A])

object Tree:

  // Exercise 2
  def size[A](t: Tree[A]): Int = t match
    case Leaf(_)             => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  

  // Exercise 3

  def maximum(t: Tree[Int]): Int = t match
    case Leaf(value) => value
    case Branch(left, right) => maximum(left) max maximum(right)

  // Exercise 4

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  
  // Exercise 5

  def fold[A,B](t: Tree[A])(f: (B, B) => B)(g: A => B): B = t match
    case Leaf(value)         => g(value)
    case Branch(left, right) => f(fold(left)(f)(g), fold(right)(f)(g))

  def size1[A](t: Tree[A]): Int = fold[A, Int](t)((x, y) => x+y+1)(_ => 1)

  def maximum1(t: Tree[Int]): Int = fold[Int, Int](t)(_ max _)(x => x)

  def map1[A, B](t: Tree[A])(f: A => B): Tree[B] = fold[A, Tree[B]](t)(Branch(_, _))(f andThen (x => Leaf(x)))

enum Option[+A]:
  case Some(get: A)
  case None

  // Exercise 6

  def map[B](f: A => B): Option[B] = this match
    case None    => None
    case Some(v) => Some(f(v))

  def getOrElse[B >: A] (default: => B): B = this match
    case None    => default
    case Some(v) => v
  

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case None => None
    case Some(v) => f(v)
  

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None => ob
    case Some(v) => Some(v)
  

  def filter(p: A => Boolean): Option[A] = this match
    case None    => None
    case Some(v) => p(v) match 
      case false  => None
      case true   => Some(v)
  

  // Scroll down for Exercise 7, in the bottom of the file, outside Option

  def forAll(p: A => Boolean): Boolean = this match
    case None => true
    case Some(a) => p(a)
    



object Option:

  // Exercise 9

  def map2[A, B, C](ao: Option[A], bo: Option[B])(f: (A,B) => C): Option[C] = 
    for {
      x <- ao
      y <- bo
      res <- Some(f(x,y))
    } yield res

  // Exercise 10

  def sequence[A](aos: List[Option[A]]): Option[List[A]] = 
    aos.foldRight[Option[List[A]]](Some(Nil))((a, b) => b.flatMap(xs => a.map(y => y :: xs)))

  // Exercise 11

  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) => acc.flatMap(x => f(a).map(y => y :: x)))
    
end Option

 

// Exercise that are outside the Option companion object

import Option.{Some, None}

def headOption[A](lst: List[A]): Option[A] = lst match
  case Nil => None
  case h:: t => Some(h)

// Exercise 7

def headGrade(lst: List[(String,Int)]): Option[Int] = 
  headOption(lst).map(_._2)


def headGrade1(lst: List[(String,Int)]): Option[Int] =
  for {
    x   <- headOption(lst)
    res <- Some(x._2)
  } yield res

// Implemented in the text book

def mean(xs: Seq[Double]): Option[Double] =
  if xs.isEmpty then None
  else Some(xs.sum / xs.length)

// Exercise 8

def variance(xs: Seq[Double]): Option[Double] =
  mean(xs).flatMap(u => mean(xs.flatMap(x => List(math.pow(x-u, 2)))))


def variance1(xs: Seq[Double]): Option[Double] = 
  for {
    u <- mean(xs)
    res <- mean(xs.flatMap(x => List(math.pow(x-u, 2))))
  } yield res
// Scroll up, to the Option object for Exercise 9
