/********************************
 * Final Exam: Advanced Programming by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2020: 10 January 2022, 15:00
 *
 * The exam consists of 13 questions to be solved within 4 hours.
 *
 * You can use  any function from the course  (textbook, exercises) in
 * the solutions, as  well as  standard library  functions.  You  can
 * access any written or electronic material, also online, but you are
 * not allowed to communicate with anybody.  By  submitting, you
 * legally declare to have solved the problems alone, without
 * communicating with anybody.
 *
 * Solve the tasks in the file 'Exam.scala' (this file) found in the
 * zip archive made available on LearnIt.
 *
 * Submit this file and only this file to learnIT. Do not convert to
 * any other format than .scala.  Do not submit the entire zip
 * archive. Do not  reorder the  answers, and  do not  remove question
 * numbers from the file.  The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * The  answers  will   be  graded  manually. We  focus  on  the
 * correctness of ideas, the use  of concepts, clarity, and style. We
 * are permissive on minor issues  such as semicolons,  commas, other
 * punctuation, small deviations  in function  names, switching
 * between  curried and not curried arguments,  etc.  We  will not
 * check whether  the type inference succeeds.   It suffices  that a
 * human reader  could infer types.
 *
 * We do not recommend solving questions to the point when they
 * compile and pass tests.  Dependency problems  and other technical
 * issues can take a lot of time, so only do this, once you are done
 * with drafting all answers.  If  you do compile,  you can use the
 * 'build.sbt' file provided  in the  zip  archive linked  above. It
 * has the  necessary library dependencies  configured, and the
 * source files from the semester are included.
 *
 * The  percentage  at  the   beginning  of  each  question  will be
 * used as a soft indicative weight in grading.
 *
 * Good luck!
 */

package adpro

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.*
import org.scalactic.Equality

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*
import fpinscala.answers.monoids.Foldable
import fpinscala.answers.parallelism.Par
import fpinscala.answers.monads.Monad
import fpinscala.answers.state.RNG.map2
import fpinscala.answers.state.RNG.map
import fpinscala.answers.state.RNG.double
import fpinscala.answers.state.RNG.int
import org.scalacheck.Prop
import adpro.Q7.size


object Q1:

  /*** WARM-UP
   *
   * Consider the following example of a simple Java class hierarchy.
   * The example is written using Scala syntax so that we do not have
   * to mix languages in the exam.  Recall that in Java all method
   * calls are virtual, so dynamically dispatched.
   *
   * abstract class Printable: 
   *   def hello(): String
   *
   * class Triangle extends Printable:  
   *   override def hello(): String = "triangle"
   *
   * class Square extends Printable: 
   *   override def hello(): String = "square"
   *
   * Now the following is an ADT in Scala, that realizes the same hierarchy:
   */

  enum Printable:
    case Triangle extends Printable
    case Square extends Printable

  /* Q1. (10 %)
   *
   * Write a function `hello` that uses pattern matching and achieves
   * the corresponding effect as calling the method hello in Java
   * implementation.  Uncomment the definition and fill in the gaps.
   */
  def hello (x: Printable): String = x match
    case Printable.Triangle => "triangle"
    case Printable.Square => "square"
  

end Q1



object Q2:

  /*** SEQUENCE AND EITHER
   *
   * Q2. (10%)
   *
   * Implement the `sequence` function for Either. The behavior
   * should be like with `sequence` for Option: at least one failure
   * (Left) in the input list should result in a failure overall, a
   * single Left. Otherwise return the Right values from the input
   * list wrapped in a single Right. For full points make sure that
   * the error value returned, if any, is the *last* error value seen
   * on the input list.
   */

  def sequence[Err,A] (as: List[Either[Err,A]]): Either[Err, List[A]] = 
    as.foldRight[Either[Err, List[A]]](Right(List.empty))((x, acc) => acc.flatMap(ls => x.map(v => v::ls)))

end Q2



object Q3:

  /*** TYPING
   *
   * Imagine that we want to implement `sequence` with Either not
   * using List but any collection F[_] for which we know that it is
   * Foldable.
   *
   * Q3. (10%)
   *
   * Write the type signature for this new function 'sequence'.
   *
   * Do not implement the function, just put '???' in the body.
   */

   // def sequence ...
   def sequence[F[_]: Foldable, A, Err](as: F[Either[Err, A]]): Either[Err, F[A]] = ???

end Q3



object Q4:

  /*** RANDOM VALUE GENERATORS
   *
   * Recall that the type State.Rand[A] is defined as follows:
   *
   * type Rand[A] = State[RNG, A]
   *
   * Q4. (10%)
   *
   * Implement a pure generator of triples, where the first two
   * components are random integers 'a' and 'b', whereas the third
   * component 'x' is a random double number between them, so the
   * following constraint is satisfied:
   *
   *     a <= x <= b
   */

  type Rand[A] = State[RNG, A]

  lazy val riid: Rand[(Int,Int,Double)] = for {
    x <- State(RNG.int)
    y <- State(RNG.int)
    a = Math.min(x,y)
    b = Math.max(x,y)
    z <- State(RNG.double)
  } yield (a,b, a + (b-a).toDouble.abs * z)

end Q4



object Q5:

  /*** TYPE EXTENSIONS
   *
   * Recall that we have two representations of Rand:
   *
   * - A type called RNG.Rand:
   *
   *   type Rand[+A] = RNG => (A, RNG)
   *
   * - And Rand as State:
   *
   *   type Rand[A] = State[RNG, A]
   *
   * In State.scala many more functions are available for RNG.Rand
   * than for State.Rand (because State is a more abstract interface
   * that can be used for other things, so it does not know that we
   * are dealing with RNG inside).  One example of such function is
   * `Rand.both`:
   *
   * def both[A,B] (ra: Rand[A], rb: Rand[B]): Rand[(A,B)]
   *
   * Q5. (5%)
   *
   * Make all functions of RNG.Rand available for State.Rand by a
   * suitable type conversion:
   */

  // ...

  // (the question continues below)
  
  // 2022 NOTE: 
  //
  // This question cannot be answered with the material we covered in
  // Scala 3. If you are interested, this is how it can be done in Scala 3 
  // https://docs.scala-lang.org/scala3/book/ca-implicit-conversions.html
  // but this material is not in the curriculum anymore

end Q5



/* Q6. (5%)
 *
 * Explain in English the mechanism you have used to achieve this. How
 * does your solution achieve the objective of Q5?
 *
 * Indicative length: 2-5 lines.
 */

// What we have used here is what we call implicit type converions. We have just implemented it with scala 3 by using the given keyword instead


object Q7:

  /*** LAZY LISTS
   *
   * Let's assume that we have the following function 'size' implemented that
   * computes the lengths of a lazy list. (The function should be correct, no
   * point to seek traps in it.)
   */
  def size[A](s: LazyList[A]): Int =
    def f(s: LazyList[A], acc: Int): Int = s match
      case LazyList.Cons(_, t) =>  f (t (), acc+1)
      case LazyList.Empty => acc
    f(s, 0)


  /* Q7. (5%)
   *
   * What is the problem with writing `size(s) >= 10` to check whether the lazy
   * list is at least 10 elements? Explain.
   */

  // We cannot be sure that the is an end to it. We could just run the function forever

end Q7



object Q8:

  import Q7.size

  /**** A BETTER SIZE FOR LAZY STREAMS
   *
   * Q8. (5%)
   *
   * Implement a pure function `checkIfLongerEqThan` that checks whether
   * a stream is longer or equal than a given bound. Do not use the standard
   * library functions lengthCompare, sizeCompare, lenghtIs, or
   * sizeIs. You can use 'size' from above, or any other functions
   * from the course.
   */

  def checkIfLongerEqThan[A](s: LazyList[A])(n: Int): Boolean = n match
    case 0 => true
    case _ => s match
      case LazyList.Empty =>  false
      case LazyList.Cons(_, t) => checkIfLongerEqThan(t())(n-1)
    
  

end Q8



object Q9:

  import Q8.checkIfLongerEqThan

  /*** TESTING
   *
   * Assume we have a solution for question Q8 (even if you skipped
   * it), so that we have a function:
   *
   *    checkIfLongerEqThan[A] (s: Stream[A]) (n: Int): Boolean
   *
   * that returns true if and only if the stream 's' has at least 'n'
   * elements.
   *
   *
   * Q9. (10%)
   *
   * Use it to write a property-based test that checks if the size of
   * every nonEmpty stream concatenated with itself is larger or equal
   * than 2.
   */

  class MySpec
    extends org.scalacheck.Properties("Q9"):

    given Arbitrary[LazyList[Int]] =
      Arbitrary { Gen.listOf(Gen.choose(1, 100))
        .map { l => LazyList(l*) } }

    property("Q9: Write the test here by replacing 'false' below") =
      Prop.forAll((x: Int, ls: LazyList[Int]) => checkIfLongerEqThan(ls)(x) == size(ls) >= x)

end Q9



object Q10:

  import Par.*

  /*** PARALLEL OPTIONS
   *
   * Q10. (10%)
   *
   * Write a function which 'flattens' a Option[Par[A]] value to a
   * Par[Option[A]] value, for any type 'A'.
   */

  def flatten[A](opa: Option[Par[A]]): Par[Option[A]] = opa match
    case None => unit(None)
    case Some(value) => value.map(Some.apply)
  

end Q10



/*** Par[Option[_]] vs Option[Par[_]]
 *
 * Q11. (5%)
 *
 * Explain in English what the function from Q10 achieves.
 * Provide its user oriented description, not an explanation of the
 * implementation.
 */

 // It creates a way to represent an option as a parallel computation 



object Q12:

  /*** MONADS
   *
   * Recall the Identity Monad, the simplest possible monad, which
   * allows unit (identity) and mapping (function application) on any
   * type.
   */

  type Id[A] = A

  given idMonad: Monad[Id] = new:
    def unit[A] (a: => A): Id[A] = a
    extension [A](a: Id[A])
      override def flatMap[B](f: A => Id[B]): Id[B] = f(a)

  /* Now assume that there is a function loop of the following type.
   **/

  def loop[A, M[_]: Monad](initial: M[A])(body: A => A)(p: A => Boolean): M[A] = ???

  /* This function runs a loop 'in the monad M'.  It starts at the
   * initial value, then it applies the function 'body' as long as the
   * produced value of the type 'A' satisfies the predicate 'p'.
   *
   *
   * The code below computes a sum of list of integers.
   * (Warning: imperative code below)
   *
   * def sum(var l: List[Int]): Int =
   *   var result = 0
   *   while l.nonEmpty do
   *     result = result + l.head
   *     l = l.tail
   *   return result
   * 
   *
   * Q12. (10%)
   *
   * Convert the above imperative implementation of 'sum' into a pure one by
   * using the 'loop' function and the identity monad.  The implementation has
   * been started for you.  Complete it by replacing '???' (you may uncomment
   * the code):
   */

  type LI[A] = (List[A], A)

  def sum (l: List[Int]): Int =
    val initial: LI[Int] = (l, 0)
    val body: LI[Int] => LI[Int] = (xs: List[Int], sum: Int) => (xs.tail, xs.head+sum)
    val p: LI[Int] => Boolean = _._1.isEmpty

    val result = loop[LI[Int], Id](initial)(body)(p)
    result._2


  /* DISCLAIMER: Normally, we do not want to compute a sum of a list in this way.
   * This is an artificial exercise for simplicity.
   */

end Q12



object Q13:

  /*** LOOPING IN A MONAD
   *
   * Q13. (10%)
   *
   * Implement the function 'loop' from the above exercise that iterates a
   * calculation in a monad. Given an initial value of type A it checks (like a
   * while loop) whether it satisfies the predicate p. If not it returns the
   * value.
   */

  def loop[A, M[_]: Monad] (initial: M[A]) (body: A => A) (p: A => Boolean): M[A] = 

    val m = summon[Monad[M]]

    initial.flatMap(x => if p(x) then 
                        loop(m.unit(body(x)))(body)(p) 
                      else m.unit(x))


end Q13
