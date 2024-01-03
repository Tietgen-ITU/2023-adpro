package adpro.solution

import org.scalacheck.{Arbitrary, Gen, Prop}
import Arbitrary.*, Prop.*
import org.scalactic.Equality

import fpinscala.answers.laziness.LazyList
import fpinscala.answers.state.*
import fpinscala.answers.monoids.Foldable
import fpinscala.answers.parallelism.Par
import fpinscala.answers.monads.Monad

object Q1:

  /*** WARM-UP
   *
   * Consider the following example of a simple Java class hierarchy.
   * The example is written using Scala syntax so that we do not have
   * to mix languages in the exam.  Recall that in Java all method
   * calls are virtual, so dynamically dispatched.
   *
   * abstract class Printable: 
   *   def hello(): String = "printable"
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
    case Triangle
    case Square

  /* Q1. (10 %)
   *
   * Write a function `hello` that uses pattern matching and achieves
   * the corresponding effect as calling the method hello in Java
   * implementation.  Uncomment the definition and fill in the gaps.
   */

  def hello(p: Printable): String = p match
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

  def sequence[Err, A](as: List[Either[Err, A]]): Either[Err, List[A]] =
    as.foldRight[Either[Err, List[A]]](Right(Nil)) { (ea, eas) =>
      for as <- eas
          a  <- ea
      yield a::as
    }
 
  // A solution without for-yield:

  def f[Err, A](ea: Either[Err, A], eas: Either[Err, List[A]]): Either[Err, List[A]] =
    eas.flatMap { l => ea.map { a => a::l } }

  def sequence_[Err,A](as: List[Either[Err, A]]): Either[Err, List[A]] =
    as.foldRight[Either[Err,List[A]]](Right(Nil))(f)

  // A nice solution with map2, unfortunately, we have not implemented map2 for
  // Either in the course (the text book did implement it, so this is probably
  // almost fine)
  // def sequence__[Err, A](as: List[Either[Err, A]]): Either[Err, List[A]] =
  //   as.foldRight[Either[Err, List[A]]](Right(Nil))((a, b) => map2(a, b)(_ :: _))

  // Some thinking on possible mistakes:
  //
  // - I think using foldLeft is likely returning the first, not the last error.
  // - Also direct recursion with a tail-recursive accumulator trick is likely
  //   giving the first error.
  // - A translation to Option is lo0sing the information about failures.
  //
  // There is this solution easy to find online:
  // https://stackoverflow.com/questions/7230999/how-to-reduce-seqeithera-b-to-eithera-seqb
  // (two TAs found it). However it uses different types List vs Seq, Err vs
  // String. Also it uses the method _.right  which was not implemented in our
  // course and it is not in std lib.  (It is a cats method):
  //
  // So I think this is still a risky exercise, worth keeping.  It can be used
  // to reward actual knowledge from last moment searching. This is the
  // stackover solution
  //
  // def sequence[A, B](s: Seq[Either[A, B]]): Either[A, Seq[B]] =
  //   s.foldRight(Right(Nil): Either[A, List[B]]) {
  //     (e, acc) => for (xs <- acc.right; x <- e.right) yield x :: xs
  //   }
  //
  // The use of Seq and B, along with .right are characteristic.
  //
  // Also it seems that acc.right and e.right is actually a mistake by the
  // poster. This will compile but .right seems unnecessary.  It just packs and
  // umpacks the Right value into RightOps needlessly.  (My solution, above,
  // does well without it).
  //
  // In the end lots of people lost points on using this stackoverflow
  // solution (it was obviously obtained by cheating, and it was
  // wrong).

end Q2



object Q3:

  /*** Typing
   *
   * Imagine that we want to implement `sequence` with Either, but using any
   * arbitrary collection F[_] for which we know that it is Foldable, instead of
   * List.
   *
   * Q3. (10%)
   *
   * Write the type signature for this new function 'sequence', enforcing a
   * suitable type constraint on F.
   *
   * Do not implement the function, just put '???' in the body.
   */
   import fpinscala.answers.monoids.Foldable

   def sequence[Err, A, F[_]: Foldable](as: F[Either[Err,A]]): Either[Err, F[A]] = ???

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

  import fpinscala.answers.state.*
  type Rand[A] = State[RNG, A]

  val riid: Rand[(Int, Int, Double)] = for
    a <- State(RNG.int)
    b <- State(RNG.int)
    l  = Math.min(a, b)
    r  = Math.max(a, b)
    x <- State (RNG.double)
  yield (l, r, l + (r-l).toDouble.abs * x)

  // A worse solution could use terribly nested maps, flatMaps (still fine, if
  // they are written as separate let-expressions or laid out readably).
  //
  // Also a (slightly) worse solution would construct `riid` from scratch using
  // RNG.nextInt, instead of composing existing generators declaratively.
  //
  // A big problem is to use imperative API, and actually create numbers
  //
  // A weak partial solution would just generate two ints and a double, without
  // ensuring any constraints between them.

end Q4



object Q5: // Scala-2 style solution

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
  
  // This solution allows to convert any State.Rand[A] value to an RNG.Rand[A]
  // value implicitly, and then all methods of RNG.Rand[A] methods can be
  // called.
  import scala.language.implicitConversions
  import fpinscala.answers.state.RNG
  import fpinscala.answers.state.State
  type Rand[A] = State[RNG, A]

  implicit def State2RNG[A] (st: Rand[A]): RNG.Rand[A] =
    st.run

  // This is a slightly less clear answer but it also makes sense (it allows to
  // use an RNG value wherever a State is needed, which has a similar effect:
  // you can work with RNG values as long as you want, using their methods, and
  // promote them to State late - this would have the right effect in the riid
  // implementation above, so I would grade it full points.
  implicit def RNGisState_[A](ra: RNG.Rand[A]): Rand[A] =
    State(ra)

  // A perfect answer (not really expected, should probably achieve both
  // conversions). This is probably hard to get right in practice, because
  // cyclic implicit conversions will break the compiler. I do not expect it
  // then.

  // A longer, but more puristic, answer is to implement a set of extension
  // methods for state that are forwarded to RNG.Rand.  This is more work but
  // has other advantages so should get full points (no issues with cyclicity of
  // implicits).
  // 
  // Note 2022: this longer solution is also a clean solution in Scala3 but 
  // would require more work than anticipated.
  //
  // Delegating just the both function is a passable solution, but weak.

end Q5



/* Q6. (5%)
 *
 * Explain in English the mechanism you have used to achieve this. How does
 * your solution achieve the objective of Q5?
 */

/* write here ... (see above for notes that amount to an answer)
 */



object Q7:

  import fpinscala.answers.laziness.LazyList

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

  // Write here ...

  // The LazyList.size function iterates the entire tail of the list
  // until it hits the empty node - this may not terminate, use a lot
  // of time, and/or a lot of memory.  This is unnecessary, because
  // after iterating 10 elements we can decide the value of the
  // predicate.

end Q7



object Q8:

  import Q7.size
  import fpinscala.answers.laziness.LazyList

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

  def checkIfLongerEqThan[A](s: LazyList[A])(n: Int): Boolean =
    size(s.take(n)) == n

  def checkIfLongerEqThan_[A](s: LazyList[A])(n: Int): Boolean =
    (n == 0) || s.drop(n-1) != LazyList.Empty

  // This solution is impure (yuck) and incorrect, because drop does not throw
  // an exception just returns empty
  def  checkIfLongerEqThan__ [A] (s: LazyList[A]) (n: Int): Boolean =
    try { s.drop(n); true } catch { case e: Throwable => false }

  // A direct recursive solution is also possible, I would like it slightly
  // less. We have directly discouraged such.

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
    extends org.scalacheck.Properties("exam-2021"):

    given Arbitrary[LazyList[Int]] =
      Arbitrary { Gen.listOf(Gen.choose(1, 100))
        .map { l => LazyList(l*) } }

    // an example solution
    property("Q9: A nonEmpty list concatenated with self is at least 2 elements long") =
      forAll { (s: LazyList[Int]) =>
        (s != LazyList.Empty) ==>
          Q8.checkIfLongerEqThan(s.append(s))(2) }

    // Some notes on possible bad solutions:
    //
    // - Testing on a single list is bad.  One should use checkIfLongerEqThan
    //   forAll, and the arbitrary list .

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
    case None => Par.unit[Option[A]](None)
    case Some(pa) => pa.map[Option[A]](Some.apply)


  // This is a complex solution that derives from the list solution in the
  // files.  In this case I find the pattern matching solution simpler.
  def flatten__[A](opa: Option[Par[A]]): Par[Option[A]] =
    opa.foldRight[Par[Option[A]]](Par.unit(None)) { (pa, poa) =>
      pa.map2(poa)((a, oa) => Some(a)) }

  // This one relies on noticing that this is sequence and reducing to the list
  // problem:
  def flatten___ [A] (opa: Option[Par[A]]): Par[Option[A]] =
    Par.sequence(opa.toList).map { _ match
      case Nil => None
      case h:: _ => Some(h)
    }

  // The variants for lists in the book code use fork, but I think they need
  // that only because they would like to parallelize list element execution.
  // In this case we do not need to parallelize - one thread is enough.
  // Whatever parallelization is needed in producing the single value of Par[A]
  // should be applied by the user at earlier stages.

end Q10



/*** Par[Option[_]] vs Option[Par[_]]
 *
 * Q11. (5%)
 *
 * Explain in English what the function from Q10 achieves.
 * Provide its user oriented description, not an explanation of the
 * implementation.
 */

 // Write here ...
 
 // This function takes a computation whose construction might have failed (the
 // computation might not be there, be triviallly None), and rewraps it into a
 // computation that produces an optional value A.  This allows composing the
 // result into a larger parallel computation expression.
 
 
 
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
   * been started for you.  Complete it by replacing '???' (you may uncomment the
   * code):
   */

  // def sum (l: List[Int]): Int = {

  //   val initial: (List[Int], Int) = ???
  //   val body = ???
  //   val p = ???

  //   val result = loop[???,???] (initial) (body) (p)
  //   result._2
  // }

  // the import from Q13 is needed to test the solution
  import Q13.loop

  // An example solution
  type LI = (List[Int], Int)

  def sum (l: List[Int]): Int =
    val initial: LI = (l, 0)
    val body: LI => LI = (l, result) => (l.tail, result + l.head)
    val p: LI => Boolean = (l, result) => l.nonEmpty
    val result: LI = loop[LI, Id](initial)(body)(p)
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

  // Both solutions below are fine
  
  def loop_[A, M[_]: Monad](initial: M[A])(body: A => A)(p: A => Boolean): M[A] =
    for
      a      <- initial
      result <- if p(a) 
                then loop_ (summon[Monad[M]].unit(body(a)))(body)(p) 
                else initial
    yield result

  def loop[A, M[_]: Monad](initial: M[A])(body: A => A) (p: A => Boolean): M[A] =
    summon[Monad[M]].flatMap(initial){ a =>
      if p(a) 
      then loop (summon[Monad[M]].unit(body(a)))(body)(p)
      else initial
    }

end Q13
