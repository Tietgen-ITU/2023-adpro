/********************************
 * Final Exam: Advanced Programming, by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2022: 04 January 2023
 *
 * The exam consists of 11 questions to be solved within 4 hours.
 *
 * You can use any function from the course (textbook, exercises) in the
 * solutions, as well as any standard library functions. You can access any
 * static written material, also online, but you are not allowed to communicate
 * with anybody or with anything (bots). Using GitHub copilot during exam is
 * not allowed. By submitting you legally declare to have solved the problems
 * alone, without communicating with anybody.
 *
 * Solve the tasks in the file 'Exam.scala' (this file) found in zip archive
 * made available on LearnIt.
 *
 * Submit this file and only this file to learnIT. Do not convert to any other
 * format than .scala. Do not submit the entire zip archive. Do not reorder the
 * answers, and do not remove question numbers from the file. The only accepted
 * file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The answers will be graded manually. We focus on the correctness of ideas,
 * the use of concepts, clarity, and style. We are permissive on minor issues
 * such as semicolons, commas, other punctuation, small deviations in function
 * names, switching between curried and not curried arguments, etc. We will not
 * check whether the type inference succeeds. It suffices that a human reader
 * could infer types.
 *
 * We do not recommend solving questions to the point when they compile and
 * pass tests. Dependency problems and other technical issues can take a lot of
 * time, so only do this, once you are done with drafting all answers. If you
 * do compile, the files are set up for scala-cli 0.1.18 with the necessary
 * library dependencies configured, and the source files from the semester are
 * included.
 *
 * The percentage included at the beginning of each question will be used as a
 * soft indicative weight in grading.
 *
 * Good luck!
 */
package adpro

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
   *   case e: T => // some code that handles the exception e of type T and 
   *                // produces a value of the same type as the main block above
   *
   * More info (but not necessary to read): 
   *
   * https://docs.scala-lang.org/scala3/book/fp-functional-error-handling.html */

  def SafeTotal[A,B](f: A => B): A => Option[B] = 

    a => {
      try
        val x = f(a)
        Some(x)
      catch 
        case e => None
    }



  /* Q2. (5%) Use `SafeTotal` to implement the `headOption` function for
   * standard library lists by turning `head` to be safe.
   *
   * Notice that this question can be solved without answering Q1. */

  def headOption[A](l: List[A]): Option[A] = 
    SafeTotal[List[A], A](_.head)(l)

end ExceptionalOptions




object PrimesAndLaziness:

  import fpinscala.answers.laziness.*
  import fpinscala.answers.laziness.LazyList.*

  /* The value `primes` below refers to a lazy list of sorted prime numbers
   * computed using the Sieve of Erastothenes implemented in the function
   * `primeFrom`. Read the code briefly and proceed to the question below.  Do
   * not worry, if you do not remember why this algorithm works. You only need
   * to roughly understand that it produces a lazy list of prime numbers. */

  def primeFrom(naturals: LazyList[Int]): LazyList[Int] =
    val p = naturals.headOption.get
    cons(p, primeFrom(naturals.filter { _ % p != 0 }))

  /** A list of the following form: `LazyList(2, 3, 5, 7, 11, ...)` */
  lazy val primes = primeFrom(from(2))



  /* Q3 (10%). Implement a function `primesApart` which uses `primes` and
   * returns a lazy list of pairs of consecutive prime numbers such that the
   * difference between the second and the first number in each pair is exactly
   * `n`, where `n` is an even number.
   *
   * For example: `primesApart(2)` should be a list of the form: 
   * `LazyList((3, 5), (5, 7), (11, 13), (17, 19), ...  )`.
   *
   * Use `primesApart` to find 
   * - The first two prime numbers `p1`, `p2` that are 10 apart (p2 - p1 == 10)
   * - The second next pair `p3, p4` with the same property. */

  def primesApart(n: Int): LazyList[(Int,Int)] = 
    primes.zip(primes.drop(1)).filter((x,y) => y-x == n)

  lazy val (p1, p2): (Int, Int) = primesApart(10).headOption.get
  lazy val (p3, p4): (Int, Int) = primesApart(10).drop(1).headOption.get


  
  /* Q4 (5%).  Explain in English how your solution uses laziness: 
   * - Name all the non-strict operators used in `primesApart` and in `primeFrom`
   * - Explain what would happen if these operators were strict instead. */

  // Write here ...


  
  /* Q5 (10%) Write a property-based test checking that for all even numbers
   * `n` such that `n <= 20` the call `primesApart(n)` generates a lazy list
   * of pairs in which the elements in the first 5 pairs are apart by `n`.
   *
   * Notice that you can still answer this question, even if you have not
   * answered questions Q3 and Q4. */

  class primesApartTest 
    extends org.scalacheck.Properties("primesApartTest"): 

    property("Elements in pairs returned by primesApart differ by n") = 
      forAll((n:Int) => ((n % 21) % 2) == 0 ==> (primesApart(n%21).take(5).exists((x,y) => y-x != (n%21)) != true))

  end primesApartTest

end PrimesAndLaziness



object ApplesToApples:

  /* Consider the type class Better[T] defined below, which allows  to choose a
   * better one between two of values of type T, for T implementing Better[T]. */

  trait Better[T]: 
    /** True if and only if `left` is "better" than `right` */
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
   * does not compile and then add necessary code before the assertion so that
   * pickBetter can be used to pick a larger apple. */

  // We have not implemented the trait Better for the type Apple

  given betterApple: Better[Apple] = new:
    def leftBetter(left: Apple, right: Apple): Boolean = left.weight > right.weight

  assert(pickBetter(bigApple, smallApple) == bigApple)



  /* Q7. (10%) Make it possible to check whether one value of T is better than
   * another value of T using an infix method `betterThan` so that the
   * following assertion compiles and is satisfied. Use the `leftBetter`
   * method.
   *
   * If you do not know how to do this for any type T, it still makes sense to
   * make it possible just for the Apple type (it will give some points).
   */

  // assert(bigApple betterThan smallApple)

end ApplesToApples



object SizedLists: 

  /* The type `SizedList` below models a linked list (like Scala's `List`) so
   * that the type system knows how long the list is. An empty list of `A`s has
   * type `SizedList[A, Null]`. A list containing a single `A` value has type
   * `SizedList[A, Inc[Null]]`. A list containing `n` values of `A` has a type
   * `SizedList[A, Inc[...[Inc[Null]]]`, where there are `n` occurrences of the
   * `Inc` nestings in the second type parameter. The lists `l0` and `l1` below
   * are examples of values of this type. */
  
  case class Inc[A]()
   
  enum SizedList[+A, S]:
    case Empty extends SizedList[Nothing, Null]
    case Cons[A, S](hd: A, tl: SizedList[A, S]) 
      extends SizedList[A, Inc[S]]

  import SizedList.*

  val l0: SizedList[Int, Null] = Empty



  /* Q8. (5%) Write the type annotation in the declaration below that
   * explicitly states the type of `l1`. Then write an expression with an 
   * explicit type annotation for a list `l2` that contains three elements 
   * 3, 1, 4. */

  val l1: SizedList[Int, Inc[Null]] = Cons(41, l0) 
 
  val l2: SizedList[Int, Inc[Inc[Inc[Null]]]] = Cons(3, Cons(1, Cons(4, Empty)))


  
  /* For `SizedList`s we can write a safe version of `head` and `tail` that
   * can only be applied to a non-empty list. The implementations are included
   * below. Read and understand them. */

  def head[A, S](l: SizedList[A, Inc[S]]): A = l match 
    case Cons(hd, tl) => hd
  
  def tail[A, S](l: SizedList[A, Inc[S]]): SizedList[A, S] = l match 
    case Cons(h, tl) => tl



  /* Q9. (10%) Write a function 'third' that given a sized list of `A`s returns
   * the third element in the list. The function should only be allowed to be 
   * called on a list containing at least three elements. */

  def third[A, S](l: SizedList[A, Inc[Inc[Inc[S]]]]): A = head(tail(tail(l)))

  /* Q10. (15%) Write a function `append` that adds an element to the end of the
   * List of type `Sized[A, S]` for any `A` and any `S`. Use recursion and respond to
   * the question in English below. */

  def append[A, S](a: A, l: SizedList[A, S]): SizedList[A, Inc[S]] = l match
    case Empty => Cons[A, S](a, Empty)
    case Cons(hd, tl) => Cons(hd, append(a, tl)) // Mark
  

  /* Mark the polymorphically recursive call in your solution. Describe in
   * English what the type parameters are instantiated to in this call. */

  // The type parameters of the recursive call is:
  // - A: It is just A
  // - S: It is SizedList[A, Inc[S]]



  /* Q11. (10%) Revisit the ADT definition of `SizedList` in the beginning of
   * this section. For *each* of the type parameters of the `SizedList` type
   * constructor state whether the constructor is invariant, covariant, or
   * contravariant in the parameter.
   *
   * Then give one example of correct type refinement here, substitute concrete
   * types for type variables, and two incorrect examples of type refinements
   * (so two examples of concrete `SizedList` types that do not refine each
   * other).  Each example should violate the variance of exactly one type
   * parameter. */ 

  // Write here ...
 
end SizedLists
