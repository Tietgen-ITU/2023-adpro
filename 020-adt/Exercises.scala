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

  // Exercise 2

  def tail[A] (l: List[A]): List[A] = ???

  // Exercise 3

  def drop[A] (l: List[A], n: Int): List[A] = ???

  // Exercise 4

  def dropWhile[A] (l: List[A], p: A => Boolean): List[A] = ???

  // Exercise 5

  def init[A] (l: List[A]): List[A] = ???

  // Exercise 6

  def length[A] (l: List[A]): Int = ???

  // Exercise 7

  def foldLeft[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???

  // Exercise 8

  def product (as: List[Int]): Int = ???

  def length1[A] (as: List[A]): Int = ???

  // Exercise 9

  def reverse[A] (l: List[A]): List[A] = ???

  // Exercise 10

  def foldRight1[A, B] (l: List[A], z: B, f: (A, B) => B): B = ???

  // Exercise 11

  def foldLeft1[A, B] (l: List[A], z: B, f: (B, A) => B): B = ???

  // Exercise 12

  def concat[A] (l: List[List[A]]): List[A] = ???

  // Exercise 13

  def filter[A] (l: List[A], p: A => Boolean): List[A] = ???

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
