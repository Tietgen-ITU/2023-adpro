package adpro.solution

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.Equality

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*
import fpinscala.answers.monoids.Foldable
import fpinscala.answers.parallelism.Par
import fpinscala.answers.monads.Monad


object ExceptionalOptions:

  import scala.collection.immutable.List

  /* Q1. (10%) Let `f` be an impure function that may throw an exception (an
   * object of a class that refines java.lang.Throwable). We want to use `f` in
   * a purely functional program without exceptions.  It would be nice to be
   * able to have a way to convert any such function to a safe function that
   * returns an `Option` value instead of throwing an exception.
   *
   * Implement function `SafeTotal` that converts a function of type `A => B`
   * into a function of type `A => Option[B]` that returns `None` whenever an
   * exception has been thrown. Otherwise it should return `Some` with a value
   * of type `B` produced by `f`.
   *
   * You will need to use impure exception handling to solve this. This is
   * allowed in question Q1. Exception handling in Scala works like in Java,
   * Here is a brief overview of the syntax:
   *
   * try
   *   // some code that may throw an exception of type T
   * catch
   *   case e: T => // some code that handles the exception e and 
   *                // produces a value of the same type as the main block above
   *
   * More info (but not necessary to read): 
   *
   * https://docs.scala-lang.org/scala3/book/fp-functional-error-handling.html
   */

  def SafeTotal[A,B] (f: A => B): A => Option[B] = 
    (a: A) =>
      try Some(f(a)) 
      catch case e: java.lang.Throwable => None

  /* Q2. (5%) Use `SafeTotal` to implement the `headOption` function for
   * standard library lists by turning `head` to be safe.
   *
   * Notice that this question can be solved without answering Q1.
   */

  def headOption[A](l: List[A]): Option[A] = 
    SafeTotal { (l: List[A]) => l.head } (l)

  // Another solution is using Try which essentially implements the above.

end ExceptionalOptions





object PrimesAndLaziness:

  import fpinscala.answers.laziness.*
  import fpinscala.answers.laziness.LazyList.*

  /* The value `primes` below refers to a lazy list of sorted prime numbers
   * computed using the Sieve of Erastothenes implemented in the function
   * `primeFrom`. Read the code briefly and proceed to the question below.  Do
   * not owrry, if you do not remember why this algorithm works. You only need
   * to roughly understand that it produces a lazy list of prime numbers. */

  def primeFrom(naturals: LazyList[Int]): LazyList[Int] =
    val p = naturals.headOption.get
    cons(p, primeFrom(naturals.filter { _ % p != 0 }))

  lazy val primes = primeFrom(from(2))

  /* Q3 (10%). Implement a function `primesApart` which uses `primes` and
   * returns a lazy list of pairs of consecutive prime numbers such that the
   * difference between the second and the first number in each pair is `n`.
   *
   * Use `primesApart` to find 
   * - the first two prime numbers `p1`, `p2` that are 10 apart (p2-p1 == 10)
   * - the second next pair `p3, p4` with the same property. */

  def primesApart(n: Int): LazyList[(Int,Int)] = 
    require(n % 2 == 0)
    cons(1, primes).zip(primes).filter { _ - _ == -n }

  lazy val (p1, p2) = primesApart(10).headOption.get
  lazy val (p3, p4) = primesApart(10).drop(1).headOption.get
  // TODO lazy val (p1, p2) = ???
  // TODO lazy val (p3, p4) = ???
  
  /* Q4 (5%).  Explain in English how your solution uses laziness: 
   * - Name all the non-strict operators used in `primesApart` and in `primeFrom`
   * - Explain what would happen if these operators were strict instead. */

  // Write here ...

  // The lazy list `primesApart(10)` generates consecutive pairs of
  // primes which differ by 10. Because it is lazy, it will only generate the
  // first two pairs. The non-strict operators used are cons, filter, and zip.
  // If they were strict the algorithm in primesFrom would `attempt` to generate
  // all prime numbers, which is countably many, and as a result we would
  // exhaust all memory (and time) before even starting to compute
  // `primesApart` or searching for a pair that is 10 apart.
  
  /* Q5 (10%) Write a property-based test checking that for all even numbers
   * `n` such that `n <= 20` the call `primesApart(n)` generates a lazyList
   * of pairs in which the elements in the first 5 pairs are apart by `n`.
   *
   * Notice that you can still answer this question, even if you have not
   * answered questions Q5 and Q6. 
   */
  class primesApartTest 
    extends org.scalacheck.Properties("primesApartTest"): 

    property("Elements in pairs returned by primesApart differ by n (solution)") = 
      val gen = Gen.choose(2, 20).suchThat { _ % 2 == 0 }
      forAll (gen) { n => primesApart(n).take(5).forAll { (a, b) => b-a == n } }

  end primesApartTest

end PrimesAndLaziness



object ApplesToApples:

  /* Consider the type class Better[T] defined below, which allows  to choose a
   * better one between two of values of type T, for T implementing Better[T]. */

  trait Better[T]: 
    /** True iff `left` is better than `right` */
    def leftBetter(left: T, right: T): Boolean 

  case class Apple(weight: Int)

  /** Returns a better of the two values provided. */
  def pickBetter[T: Better](left: T, right: T): T = 
    if summon[Better[T]].leftBetter(left, right) 
    then left 
    else right

  val bigApple = Apple(1000)
  val smallApple = Apple(10)

  /* Q6 (10%). The assertion below does not compile. Explain in English why it
   * does not compile and then add necessary code before te assertion so that
   * pickBetter can be used to pick a larger apple.
   */

  // Write here ... 

  // The compilation fails because pickBetter requires that the type T
  // implements the type class Better (so an instance of Better[T] exists).
  // However no such instance exists in this module. To fix this we need 
  // to provide an instance (a given). 

  given betterApple: Better[Apple] = new Better[Apple]:
    def leftBetter(left: Apple, right: Apple): Boolean = 
      left.weight >= right.weight

  assert(pickBetter(bigApple, smallApple) == bigApple)

  /* Q7. (10%) Make it possible to check whether one value of T is better than
   * another value of T using an infix method `betterThan` so that the
   * following assertion compiles and is satisfied. Use the `leftBetter`
   * method.
   *
   * If you do not know how to do this for any type T, it still makes sense to
   * make it possible just for Apple for partial points.
   */

  // The general solution
  extension [T: Better] (self: T) 
    infix def betterThan(that: T): Boolean =
      summon[Better[T]].leftBetter(self, that)

  // The specialized solution for Apple
  extension (self: Apple) 
    infix def betterThan(that: Apple): Boolean =
      betterApple.leftBetter(self, that)

  assert(bigApple betterThan smallApple)

end ApplesToApples



object SizedLists: 

  /* The type SizedList below models a linked list (like Scala's List) so that
   * the type system knows how long the list is. An empty list of As has type
   * SizedList[A, Null]. A list containing a single A value has type
   * SizedList[A, Inc[Null]]. A list containing n values of A has a type
   * SizedList[A, Inc[...[Inc[Null]]], where there are n occurences of the Inc
   * nesting in the second type parameter. The lists l0 and l1 below are
   * examples of values of this type. */
  
  case class Inc[A]()
   
  enum SizedList[+A, S]:
    case Empty extends SizedList[Nothing, Null]
    case Cons[A, S](hd: A, tl: SizedList[A, S]) 
      extends SizedList[A, Inc[S]]

  import SizedList.*

  val l0: SizedList[Int, Null] = Empty



  /* Q8. (5%) Write the type annotation in the declaration below that
   * explicitly states the type of l1. Then write an expression, and the
   * explicit type annotation, for a list l2 that contains three elements 
   * 3, 1, 4. */

  val l1: SizedList[Int, Inc[Null]] = Cons(41, l0)
 
  val l2: SizedList[Int, Inc[Inc[Inc[Null]]]] = Cons(3, Cons(1, Cons(4, Empty)))


  
  /* For SizedLists we can write a safe version of `head` and `tail` that
   * can only be applied to nonEmpty list. The implementations are included
   * below. Read and understand them. */

  def head[A, S](l: SizedList[A, Inc[S]]): A = l match 
    case Cons(hd, tl) => hd
  
  def tail[A, S](l: SizedList[A, Inc[S]]): SizedList[A, S] = l match 
    case Cons(h, tl) => tl



  /* Q9. (10%) Write a function `third` that given a sized list of As returns
   * the third element in the list. The function should only be allowed to be 
   * called on a list containing at least three elements. */

  def third[A, S](l: SizedList[A, Inc[Inc[Inc[S]]]]): A = 
    head(tail(tail[A, Inc[Inc[S]]](l)))




  /* Q10. (15%) Write a function 'append' that adds an element to the end of the
   * List of type Sized[A, S] for any A and any S. Use recursion and repond to
   * the question in English below. */

  def append[A, S](a: A, l: SizedList[A, S]): SizedList[A, Inc[S]] = l match 
    case Empty => Cons(a, Empty) 
    case Cons(h, tl) => Cons(h, append(a, tl))

  /* Mark the polymorphically recursive call in your solution. Describe in
   * English what the type parameters are instantiated to in this call. */

  // The only call to append in the body is recursive and polymorphically
  // recursive. It is difficult to name its type parameters, as there is no way
  // to name them explicitly in the code---they are inferred by the type
  // checker. The call is append[A, S2] where S2 is some type which has a
  // property that Inc[S2] = S. Such a type exists in this case of pattern
  // matching (but not in the otehr case), and I am somewhat impressed that
  // Scala can infer that.


  /* Q11. (10%) Revisit the ADT definition of `SizedList` in the begining of the
   * question. For *each* of the type parameters of the SizedList type
   * constructor state whether the constructor is invariant, covariant, or
   * contravariant in the parameter.
   *
   * Then give one example of correct type refinement here, substitute concrete
   * types for type variables, and two incorrect examples of type refinements
   * (so two examples of concrete SizedList types that do not refine each
   * other).  Each example should violate the variance of exactly oen type
   * parameter. */ 

  // SizedList is covariant in A and invariant in the S parameter. 
  //
  // An example of correct refinement: 
  // SizedList[Nothing, Inc[Null]] <: SizedList[Int, Inc[Null]]
  //
  // An example of an incorrect refinement, violating the variance of the A
  // parameter:
  // SizedList[Int, Null] <: SizedList[Nothing, Null]
  //
  // An example of an incorrect refinement, violating the variance of the S
  // parameter:
  // SizedList[Int, Null] <: SizedList[Int, Inc[Inc[Null]]]

end SizedLists
