// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.adt

import java.util.NoSuchElementException

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])


object List:

  def head[A] (l: List[A]): A = l match
    case Nil => throw NoSuchElementException()
    case Cons(h, _) => h

  def apply[A] (as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def append[A] (l1: List[A], l2: List[A]): List[A] =
    l1 match
      case Nil => l2
      case Cons(h, t) => Cons(h, append(t, l2))

  def foldRight[A, B] (l: List[A], z: B, f: (A, B) => B): B = l match
    case Nil => z
    case Cons(a, as) => f(a, foldRight(as, z, f))

  def map[A, B] (l: List[A], f: A => B): List[B] =
    foldRight[A, List[B]] (l, Nil, (a, z) => Cons(f(a), z))

  // Exercise 1 (is to be solved without programming)
  /*
  This returns 3. Because it matches the third case. Even though that it would match some of the other cases
  then it uses the first that matches and executes the right hand part of it.
  */

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(head, tail) => tail
  

  // Exercise 3

  def drop[A] (l: List[A], n: Int): List[A] = n match
    case x if x <= 0 => l
    case x => drop(tail(l), x-1)

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = l match
    case Nil          => Nil
    case Cons(x, xs)  => p(x) match 
                          case true => dropWhile(xs,p)
                          case false => l

  // Exercise 5

  def init[A] (l: List[A]): List[A] = l match
    case Nil => throw NoSuchElementException()
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  

  // Exercise 6

  def length[A] (l: List[A]): Int =
    foldRight(l, 0, (_,b) => b+1)

  // Exercise 7

  @annotation.tailrec
  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
    l match
      case Nil          => z
      case Cons(x, xs)  => foldLeft(xs, f(z, x), f)

  // Exercise 8

  def product (as: List[Int]): Int = 
    foldLeft(as, 1, (acc, x) => acc * x)

  def length1[A] (as: List[A]): Int = 
    foldLeft(as, 0, (acc, _) => acc+1)

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = 
    foldLeft[A,List[A]](l, Nil, (xs, x) => Cons(x,xs))

  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = 
    def aux(a: List[A], c: B => B): B = a match 
      case Nil => c(z)
      case Cons(x, xs) => aux(xs, (acc:B) => c(f(x, acc))) 

    aux(l, a => a)

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = 
    foldRight1[A, B => B](l, a => a, (x,y) => b => y(f(b,x)))(z)

  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] =
    foldRight1[List[A], List[A]](l, Nil, append)

  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = 
    foldRight[A, List[A]](l, Nil, (x, acc) => p(x) match
      case true => Cons(x, acc)
      case false => acc
    )

  // Exercise 14

  def flatMap[A,B] (l: List[A], f: A => List[B]): List[B] = ???

  // Exercise 15

  def filter1[A] (l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 16

  def addPairwise (l: List[Int], r: List[Int]): List[Int] = ???

  // Exercise 17

  def zipWith[A, B, C] (l: List[A], r: List[B], f: (A,B) => C): List[C] = ???

  // Exercise 18

  def hasSubsequence[A] (sup: List[A], sub: List[A]): Boolean = ???
