// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.state

import adpro.lazyList.LazyList
import adpro.lazyList.LazyList.*
import adpro.state.RNG.int
import adpro.state.RNG.SimpleRNG


trait RNG:
  /** Generate a random `Int`. We define other functions using `nextInt`. */
  def nextInt: (Int, RNG) 

object RNG:

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      // `&` is bitwise AND. We use the current seed to generate a new seed.
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL 
      // The next state, which is an `RNG` instance created from the new seed. 
      val nextRNG = SimpleRNG(newSeed)
      // `>>>` is right binary shift with zero fill. 
      // The value `n` is our new pseudo-random integer.
      val n = (newSeed >>> 16).toInt 
      // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
      (n, nextRNG) 


  // Exercise 1

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (x, next) = rng.nextInt
    if x == Int.MinValue then nonNegativeInt(next) else (x.abs, next)

  // Exercise 2

  def double(rng: RNG): (Double, RNG) = 
    val (res, next) = nonNegativeInt(rng)
    ((res.toDouble / Int.MaxValue), next)
    

  // Exercise 3
  
  // The return type is broken and needs to be fixed
  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (x, next) = nonNegativeInt(rng)
    val (y, rngy) = double(next)

    ((x,y), rngy)
    

  // The return type is broken and needs to be fixed
  def doubleInt(rng: RNG): ((Double, Int), RNG) = 
    val ((x, y), next) = intDouble(rng)

    ((y,x), next)

  // Exercise 4

  // The return type is broken and needs to be fixed
  def ints(size: Int)(rng: RNG): (List[Int], RNG) = 
    size match 
      case 0 => (Nil, rng)
      case n => 
        val (x, next) = rng.nextInt
        val (xs, fin) = ints(n-1)(next)
        (x :: xs, fin)


  type Rand[+A] = RNG => (A, RNG)

  lazy val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeEven: Rand[Int] = map(nonNegativeInt) { i => i - i % 2 }

  // Exercise 5

  lazy val double2: Rand[Double] = 
    map(nonNegativeInt)(x => (x.toDouble / Int.MaxValue))
    

  // Exercise 6

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = 
    rng => {
      val (x, rngx) = ra(rng)
      val (y, rngy) = rb(rngx)

      (f(x,y), rngy)
    }

  // Exercise 7

  def sequence[A](ras: List[Rand[A]]): Rand[List[A]] =
    ras.foldRight[Rand[List[A]]](unit(Nil))((a, b) => map2(a,b)((x,y) => x::y))

  def ints2(size: Int): Rand[List[Int]] =
    val l = List.fill[Rand[Int]](size)(rng => rng.nextInt)
    sequence(l)

  // Exercise 8

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (x, next) = f(rng)
      val (y, fin) = g(x)(next)
      (y, fin)
    }

  def nonNegativeLessThan(bound: Int): Rand[Int] =
    flatMap(nonNegativeInt)(x => unit(x % bound))

end RNG

import State.*

case class State[S, +A](run: S => (A, S)):

  // Exercise 9 (methods in class State)
  // Search for the second part (sequence) below
  
  def flatMap[B](f: A => State[S, B]): State[S, B] = 
    State{
      s => {
        val (x, ns) = this.run(s)
        val (y, ys) = f(x).run(ns)
        (y, ys)
      }
    }

  def map[B](f: A => B): State[S, B] = 
    State {
      s => {
        val (a, as) = this.run(s)
        (f(a), as)
      }
    }

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = 
    State{
      s => {
        val (x, xs) = this.run(s)
        val (y, ys) = sb.run(xs)

        (f(x,y), ys)
      }
    }


object State:

  def unit[S, A](a: A): State[S, A] =
    State { s => (a, s) }

  def modify[S](f: S => S): State[S, Unit] = for
    s <- get // Gets the current state and assigns it to `s`.
    _ <- set(f(s)) // Sets the new state to `f` applied to `s`.
  yield ()

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Now Rand can be redefined like this (we keep it here in the State object,
  // to avoid conflict with the other Rand in RNG).
  type Rand[A] = State[RNG, A]

  // Exercise 9 (sequence, continued)
 
  def sequence[S,A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight[State[S,List[A]]](unit(Nil))((x, y) => x.map2(y)((a,b) => a::b))

  import adpro.lazyList.LazyList

  // Exercise 10 (stateToLazyList)
  
  def stateToLazyList[S, A](s: State[S,A])(initial: S): LazyList[A] =
    val (x, nexts) = s.run(initial)
    cons(x, stateToLazyList(s)(nexts))

  // Exercise 11 (lazyInts out of stateToLazyList)
  
  def lazyInts(rng: RNG): LazyList[Int] = 
    stateToLazyList(State[RNG, Int](s => s.nextInt ))(rng)

  lazy val tenStrictInts: List[Int] = 
    lazyInts(SimpleRNG(0)).take(10).toList

end State
