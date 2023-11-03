/************************************************************************
  Final Exam: Advanced Programming (Master-Level, MSc-Level)
  IT University of Copenhagen, Autumn 2019: 16 December 9:00
  Andrzej Wąsowski
 ************************************************************************

  Your Full Name: ___
  Your ITU email account: ___

  The exam consists of 3 parts encompassing 15 qpuestions to be solved
  within 4 hours.   MSc students solve all parts.   Master of Software
  Engineering students only solve the first  2 parts.  You can use any
  function from the course (textbook,  exercises) in the solutions, as
  well as standard  library functions.  You can access  any written or
  electronic  materials,  also online,  but  you  are not  allowed  to
  communicate with anybody during the exam.

  By  submitting,  you declare  to  have  solved the  problems  alone,
  without communicating with anybody.



  SUBMISSION

  Solve the tasks in the file 'Exam2019Autumn.scala' (this file) found
  in the zip archive made available on LearnIt.

  Fill in your name and your ITU email above, in the top of the file.

  Submit this file  and only this file to learnIT.   Do not convert it
  to  any other  format than  .scala.  Do  not submit  the entire  zip
  archive. Do  not reorder  the answers,  and do  not remove  question
  numbers from the  file.  When free text answers  are expected, write
  them as comments.

  The only accepted file format is '.scala'.

  The  answers  will   be  graded  manually. We  will   focus  on  the
  correctness of ideas and the use  of the course concepts. We will be
  permissive on  minor issues  such as semicolons,  other punctuation,
  small deviations  in function  names, switching between  curried and
  uncurried  parameters, etc.   We  will not  check  whether the  type
  inference  succeeds. It suffices  that  a human  reader could  infer
  types.



  ADVICE

  We do not recommend solving questions to the point when they compile
  and pass tests.  Dependency problems  and other technical issues can
  take a lot of time, so only do this, once you are done with drafting
  all answers.

  Nevertheless, if  you do compile,  you can use the  'build.sbt' file
  provided  in the  zip  archive linked  above. It  has the  necessary
  library dependencies  configured. The zip archive also  contains the
  course libraries that the solutions depend on.

  Some values are made lazy  so prevent your executions failing before
  all exercises are solved (this is  too help you running in the REPL,
  or  testing otherwise.   Otherwise  these lazy  annotations have  no
  addition meaning.

  Good luck!

************************************************************************1G*/

package adpro

import scala.language.higherKinds
import scala.language.implicitConversions

import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.Checkers
import org.scalacheck.Prop._

import org.scalacheck.Arbitrary
import org.scalatest.prop._

import com.cra.figaro.language.{Element, Constant, Flip, Universe, Select}
import com.cra.figaro.library.compound._
import com.cra.figaro.library.atomic.continuous.{Beta, AtomicBeta}
import com.cra.figaro.library.atomic.discrete.{Binomial,Uniform}
import com.cra.figaro.algorithm.ProbQueryAlgorithm
import com.cra.figaro.algorithm.sampling.{Importance}
import com.cra.figaro.algorithm.factored.{VariableElimination}

import fpinscala.monoids.Monoid
import fpinscala.monads._
import fpinscala.laziness.{Stream,Empty,Cons}
import fpinscala.laziness.Stream._
import fpinscala.parallelism._
import fpinscala.parallelism.Par._
import adpro.data._
import adpro.data.FingerTree._
import monocle.{Lens,Optional}

object Exam2019AutumnSolution {

  object Game {

    /**
     * Part 1. ROCK, PAPER, SCISSORS (Warmup)
     *
     * Consider the game of rock, paper, scissors. Two players independently and
     * simultanously select a move.  The possible moves are Rock, Paper, and
     * Scissors.  The winning is established using the following rules:
     *
     * Rock wins against Scissors
     * Paper wins against Rock
     * Scissors win against Paper
     * Otherwise there is a draw.
     */

    sealed trait Move
    case object Rock extends Move
    case object Paper extends Move
    case object Scissors extends Move

    sealed trait Player
    case object P1 extends Player // Player 1
    case object P2 extends Player // Player 2

    /**
     * Q1. Implement function winner, that given the two moves selected by
     * players decides, who is the winner (Some), or wether there is a draw
     * (None)
     */

    type Result = Option[Player]

    // def winner (player1: Move, player2: Move): Result = ???

    def winner (player1: Move, player2: Move): Result =
      (player1, player2) match {
        case (Rock, Scissors) => Some (P1)
        case (Scissors, Rock) => Some (P2)
        case (Paper, Rock) => Some (P1)
        case (Rock, Paper) => Some (P2)
        case (Scissors, Paper) => Some (P1)
        case (Paper, Scissors) => Some (P2)
        case _ => None
      }

    /**
     *  Let us assume that players use very simple strategies to play the game.
     *  A strategy selects the next move ignoring the past behaviour of the
     *  oponent, but may take into account your own past behavior.  We represent
     *  a strategy as an infinite stream of moves: Stream[Move].  The stream
     *  basically says which moves a player will do in order (of course the
     *  other player does not know this strategy).
     *
     *  Q2. Define two strategies: alwaysScissors - a strategy of a player who
     *  always plays Scissors, and alternating - a strategy of  a player who
     *  plays always in the same alternation: Rock -> Paper -> Scissors -> Rock
     *  -> Paper -> Scissors -> ...
     */

    // def alwaysScissors: Stream[Move] = ???

    // def alternating: Stream[Move] = ???

    def alwaysScissors: Stream[Move] = Stream.constant (Scissors)

    def alternating: Stream[Move] = cons (Rock, cons (Paper, cons (Scissors, alternating)))


    /**
     * Given two strategies, one for each player, we can simulate many games,
     * and establish who is winning on average.
     *
     * Q3. Compute an infinite stream of results, Stream[Result], given two
     * input strategies: 'alwaysScissors' for Player 1 and 'alternating' for
     * Player 2.
     *
     * You can reuse the function 'winner' from above, and the two streams
     * defined in Q2.  If you have not solved Question Q1 or Q2, then just
     * assume that the definitions exist.  You do not need to run them to solve
     * this question.
     */

    // lazy val results: Stream[Result] = ???

    lazy val results: Stream[Result] =
     alwaysScissors
       .zip (alternating)
       .map { case (p1,p2) => winner (p1,p2) }

    /**
     * Q4. Compute the fraction how many times the Player 1 (alwaysScissors)
     * wins on average again Player 2 (alternating), using a prefix of 500
     * entries.  You do not have to compute the actual value. Just show the
     * code that computes it.
     *
     * Reuse the stream results from Q3.  Assume that the stream is defined,
     * if you have not solved the question.
     */

    // lazy val scissorsFraction: Double = ???

    lazy val scissorsFraction: Double =
      results
       .take (500)
       .filter { _ == Some (P1) }
       .toList
       .size / 500.0


    /**
     * Now we change from deterministic strategies to probabilistic strategies,
     * so instead of a stream (Stream[Move]), a strategy is a distribution
     * indicating what are probabilities of various next moves (Element[Move]).
     *
     * Q5.Create two strategies, Alice and Bob. Alice uniformly picks up Rock,
     * Paper, and Scissors.  Bob Picks Rock and Paper with probability 0.5 each,
     * and never picks Scissors.
     *
     * Links to relevant documentation (if you forgot from the course):
     *
     * https://www.cra.com/Figaro_ScalaDoc/com/cra/figaro/library/atomic/discrete/Uniform$.html
     * https://www.cra.com/Figaro_ScalaDoc/com/cra/figaro/language/Select$.html
     */

    type Strategy = Element[Move]

    // lazy val Alice: Strategy = ???
    lazy val Alice: Strategy = Uniform (Rock, Paper, Scissors)

    // lazy val Bob: Strategy = ???
    lazy val Bob: Strategy = Select (0.5 -> Rock, 0.5 -> Paper, 0.0 -> Scissors)

    /**
     * Q6. Implement a function that computes a result of a game given two
     * probabilistic strategies. This function will give a probability of
     * winning for each player, and a probability of a draw.
     *
     * Again, assume that the function 'winner' defining the outcome is
     * available.
     */

    //def game (player1: Strategy, player2: Strategy): Element[Result] = ???

    def game (player1: Strategy, player2: Strategy): Element[Result] = for {
      p1 <- player1
      p2 <- player2
    } yield winner (p1,p2)

    /*
     * Q7. Use importance sampling to estimate the probability that Alice is
     * winning when playing against Bob.
     *
     * You do not have to report the number, just show the scala code that
     * computes the probability.
     */

    //lazy val aliceFraction: Double = ???

    lazy val aliceFraction: Double =
      Importance.probability (game (Alice, Bob), Some(P1))
  }



  object MonadsAndTesting {

    /**
     * Part 2. MONADS AND TESTING
     *
     * We introduce a simple Monad, called Trivial.  The Trivial monad just add
     * unit and flatMap to *any* data type.  The implementation is given below.
     */

/* 01 */ case class Trivial[A] (a: A) {
/* 02 */   def flatMap[B]  (f: A => Trivial[B]): Trivial[B] = f(a)
/* 03 */   def map[B]  (f: A => B): Trivial[B] =
/* 04 */     flatMap[B] (a => Trivial.unit (f(a)))
/* 05 */ }
/* 06 */
/* 07 */ object Trivial {
/* 08 */   def unit[A] (a: A) = Trivial (a)
/* 09 */   def flatMap[A,B] (ma: Trivial[A]) (f: A => Trivial[B]): Trivial[B] =
/* 10 */     ma flatMap f
/* 11 */   def map[A,B]  (ma: Trivial[A]) (f: A => B): Trivial[B] =
/* 12 */     ma map f
/* 13 */ }

    /**
     * Q8. Explain why flatMap is implemented twice above? Write your answer
     * below in a comment, in English or Danish. Expected length: 5-10 lines.
     * Lines have been numbered in the program above to help you refer to lines
     * unambigously, if you wanted to.
     */

    // ...

    // The actual implementation of flatMap is found in the class Trivial (line
    // 02). This function just applies f to the value stored in the monad. The
    // second flatMap, in the companion object Trivial (line 09), delegates
    // to the first one.  It is only added, so that both a function-style and
    // a method-style invocation is supported:
    //
    //  method style:    Trivial (1).flatMap (f)
    //  function style:  flatMap (Trivial(1)) (f) -- after a suitable import.


    /**
     * We shall now be testing whether a Trivial is actually a monad.  A
     * property testing class is set up below. And the first law written is the
     * identity law.
     */

    class TrivialSpec extends FreeSpec with PropertyChecks with Matchers {

      "Trivial Monad is a monad" - {

        val TM = Trivial

        // This code will be removed in the question code (this is the answer
        // for Q9 below).

        implicit def arbTrivial[A: Arbitrary]: Arbitrary[Trivial[A]] =
          Arbitrary {
            for { a <- implicitly[Arbitrary[A]].arbitrary }
            yield Trivial.unit (a)
          }

        "Trivial monad satisfies the identity law" in {
          forAll { (tmx: Trivial[Int]) => Trivial.map (tmx) (identity) shouldBe tmx }
        }

        /**
         * The test above fails to compile with the following error message:
         *
         * could not find implicit value for parameter arbA:
         * org.scalacheck.Arbitrary[adpro.Exam2019AutumnSolution.MonadsAndTesting.Trivial[Int]]
         *
         *   forAll { (tmx: Trivial[Int]) => Trivial.map (tmx) (identity) shouldBe tmx }
         *          ^
         *
         * Q9. Write the missing piece of code that fixes the error message
         * (namely show evidence that an instance of Arbitrary for Trivial[Int]
         * exists). For best grade, write this evidence for Trivial[A] and make
         * the compiler find the evidence for Arbitrary[Trivial[Int]] by itself.
         */

        // ... write here (uncomment, if you want)

        /**
         * Q10. A monad must also satisfy the associativity law. Write a
         * property test that checks that Trivial[Int] is indeed associative
         * (using the standard definition of the associative law for Monads).
         */

        "Trivial monad is associative" in {

          // ...

          // this property will be removed

          forAll { (tmx: Trivial[Int],
            f: Int => Trivial[Int],
            g: Int => Trivial[Int] ) =>
              TM.flatMap[Int,Int] (TM.flatMap (tmx) (f)) (g) shouldBe
                TM.flatMap (tmx) (x => TM.flatMap (f(x)) (g))
          }
        }

        /**
         * We can construct an instance of Trivial[A] for any concrete value a:
         * A  by just calling Trivial(a).  Then we can call map and flatMap on
         * this instance:
         *
         *    Both 'Trivial(a).map _' and 'Trivial(a).flatMap _' are legal calls.
         *
         * Q11. Use a suitable mechanism in Scala to inject map and flatMap as
         * extension methods for any type A, so that any type can be seen as a
         * trivial monad, ie. so whatever A is for any value a: A it is legal to
         * write
         *
         *   Both 'a.map _' and 'a.flatMap _'
         *
         * The meaning of the above calls should be
         * the same as in the monad Trivial[A], and the solution should work
         * without modifying the type A (A is a generic type parameter).
         */

        // ... (write here)

        implicit def toTrivial[A] (a: A): Trivial[A] = Trivial(a)

        /**
         * Q12. Add support for implicit unwrapping of trivial values so that
         * the expressions below are type correct:
         *
         * val x1: Int = 1.map (identity)
         * val x2: Int = 2.flatMap (x => Trivial.unit (3))
         *
         * Hint: Note that without implicit unwrapping, only the following expressions
         * are type correct:
         *
         * val x1: Trivial[Int] = 1.map (identity)
         * val x2: Trivial[Int] = 2.flatMap (x => Trivial.unit (3))
         */

        // ...

        implicit def toNontrivial[A] (ta: Trivial[A]): A = ta match {
          case Trivial(a) => a
        }

      }

    }

  }


  object Lenses {

  /**
   * Part 3. LENSES AND FINGER TREES
   *
   * (Skip this if you are a Master of Software Engineering Student. If this
   * sounds mysterious then do not skip.  Students enrolled in MSc in Computer
   * Science, and in MSc in Software Design, and single subject students will be
   * graded on this questions)
   *
   * We are interested in creating a lens for finger trees that would allow to
   * access the second (from the left) element in the tree.
   *
   * Q13. Will such a lens be total or partial? And will it be then modeled as an
   * Optional or as a Lens in the Monocle library? Explain below in English or
   * Danish.  Estimated length of the answer: 2-6 lines.
   */

  // ...

  // The lens will be partial, because for trees shorter than 2 elements there
  // is no value to return.  This means that it will be modeled as an Optional
  // using the Monocle library.

  /**
   * Q14. Implement the above lens, peekLL.  Note that the type below (TTT)
   * needs to be replaced with the type of the Monocle lens, as you decided in
   * Q13.
   *
   * When the tree is empty, and we need to set a value through this lens,
   * we need to invent the first element as well. Just use the same element
   * twice, both for the first left-most, and the second-leftmost position.
   */



   // def peekLL[A] = {

   //   def get (t: FingerTree[A]) = ???

   //   def set (a: A) (t: FingerTree[A]) = ???

   //   // Uncomment one of the following lines, and delete the other one, based on
   //   // your answer to Q13:

   //   // Optional (get) (set)
   //   // Lens (get) (set)
   // }


   def peekLL[A]: Optional[FingerTree[A],A] = {

     def getOption (t: FingerTree[A]): Option[A] = t match {
       case ConsL (_, ConsL (h, _)) => Some (h)
       case _ => None
     }

     def set (a: A) (t: FingerTree[A]): FingerTree[A] = t match {
       case ConsL (h, ConsL (_, tl)) => addL (h, addL(a,tl))
       case ConsL (h,tl) => addL (h,Single (a))
       case _ => t.addL (a).addL (a)
     }

     Optional (getOption) (set)
   }

   // These are additional examples that will be removed from the question. A
   // lens that accesses the rightmost element:

   def peekR[A]: Optional[FingerTree[A],A] = {

     def getOption (t: FingerTree[A]): Option[A] = t match {
       case ConsR (_,h) => Some (h)
       case _ => None
     }

     def set (a: A) (t: FingerTree[A]) = t match {
       case ConsR (t,h) => addR(t,a)
       case _ => Single (a)
     }

     Optional (getOption) (set)
   }

   // These are additional examples that will be removed from the question. A
   // lens that accesses the second rightmost element:

   def peekRR[A]: Optional[FingerTree[A],A] = {

     def getOption (t: FingerTree[A]): Option[A] = t match {
       case ConsR (ConsR(_,h), _) => Some (h)
       case _ => None
     }

     def set (a: A) (t: FingerTree[A]): FingerTree[A] = t match {
       case ConsR (ConsR(tl,_), h) => addR(addR(tl,a),h)
       case ConsR (tl,h) => addL (a,Single (h))
       case _ => t.addL (a).addL (a)
     }

     Optional (getOption) (set)
   }

   /**
    * Q15. For your solution to Q14 explain in English (or Danish) whether your
    * implementation of peekLL satisfies the GetPut and PutGet laws.
    *
    * Assume that two finger trees are equal if they store the same elements in
    * the same order, so our notion of equality checks:
    *
    *      t1.toList == t2.toList
    *
    * Estimated size of the answer 10-20 lines.
    */

   // ...

   // The peekLL satisfies the GetPut law. We split the argument into cases:
   //
   // 1. If the tree is empty or singleton, the get call fails, and the law is
   //    satisfied trivially
   // 2. If the tree is at most two elements, then we get the second element and
   //    put it back without any change.
   //
   // The peekLL satisfies the PutGet law. We split the argument into cases:
   //
   // 1, If the tree t is empty, the call put(a)(t) creates a tree  holding
   //    <a,a>. Get from that tree returns an 'a'. So we pass.
   // 2. If the tree t is a singleton tree holding a 'b', the call put(a)(t)
   //    creates a tree holding <b,a> and get retrieves 'a' again. So we pass.
   // 3. If the tree t is longer, then the put and get operate on the same
   //    element without changing the other elements in the tree.  Again we
   //    receive an 'a' from get and satisfy the law.

  }

}


