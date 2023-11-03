/************************************************************************
  Final Exam: Advanced Programming (Master-Level, MSc-Level)
  IT University of Copenhagen, Autumn 2019: 16 December 9:00
  by Andrzej Wąsowski

  Your Full Name: ___
  Your ITU email account: ___

  The exam consists of 3 parts  encompassing 15 questions to be solved
  within 4 hours.  MSc students solve all parts (K-CS, K-SDT, guests).
  Master of Software Engineering students (M-SEN) solve only the first
  2 parts.

  The three parts are completely  independent.  Within the parts it is
  possible  to answer  later questions,  missing the  answers for  the
  previous ones, but it is recommended to answer questions within part
  in order.  This should be most efficient.

  You can  use any function  from the course (textbook,  exercises) in
  the  solutions, as  well  as standard  library  functions.  You  can
  access any written or electronic  material, also online, but you are
  not allowed to communicate with anybody during the exam.

  By  submitting,  you declare  to  have  solved the  problems  alone,
  without communicating with anybody.

  SUBMISSION

  Solve the tasks in the file 'Exam2019Autumn.scala' (this file) found
  in the zip archive made available on LearnIt.

  Fill in your name and your ITU email above, in the top of the file.

  Submit this file  and only this file to learnIT.   Do not convert it
  to  any other  format than  .scala.  Do  not submit  the entire  zip
  archive. Do  not reorder  the answers,  and do  not remove  question
  numbers from the  file.

  The only accepted file format is '.scala'.

  Keep the solutions within 80 columns width to facilitate grading.

  ADVICE

  The  answers  will   be  graded  manually. We  will   focus  on  the
  correctness of ideas and the use  of the course concepts. We will be
  permissive on  minor issues  such as semicolons,  other punctuation,
  small deviations  in function  names, switching between  curried and
  not  curried arguments,  etc.  We  will not  check whether  the type
  inference succeeds.   It suffices  that a  human reader  could infer
  types.

  We do not recommend solving questions to the point when they compile
  and pass tests.  Dependency problems  and other technical issues can
  take a lot of time, so only do this, once you are done with drafting
  all answers.

  Nevertheless, if  you do compile,  you can use the  'build.sbt' file
  provided  in the  zip  archive linked  above. It  has the  necessary
  library dependencies  configured. The zip archive also  contains the
  course libraries that the solutions depend on.

  Good luck!

*************************************************************************/

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

object Exam2019Autumn {

  object Game {

    /**
     * Part 1. ROCK, PAPER, SCISSORS
     *
     * Consider the old Chinese game of rock, paper, and scissors. Two
     * players independently  and simultaneously  select a  move.  The
     * possible moves are  Rock, Paper, and Scissors.   The winning is
     * established using the following rules:
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
     * Q1. Implement  the  function  'winner' that,  given  the  moves
     * selected by two players, decides,  who is the winner (Some), or
     * if there is a draw (None).
     */

    type Result = Option[Player]

    def winner (player1: Move, player2: Move): Result = ???

    /**
     *  Assume that  players use  very simple  strategies to  play the
     *  game.   A strategy  selects the  next move  ignoring the  past
     *  behavior of the  opponent, but may take into  account your own
     *  past behavior.  We represent a  strategy as an infinite stream
     *  of moves: Stream[Move].
     *
     *  The stream basically says in  which order the player will make
     *  what moves.   Of course, the  other player does not  know this
     *  strategy.
     *
     *  Q2. Define two strategies:
     *
     *  a) alwaysScissors:  A strategy  of a  player who  always plays
     *  Scissors, and
     *
     *  b) alternating: A strategy of a player who plays always in the
     *  following alternation: Rock  -> Paper  -> Scissors ->  Rock ->
     *  Paper -> Scissors -> ...
     */

    def alwaysScissors: Stream[Move] = ???

    def alternating: Stream[Move] = ???

    /**
     * Given two strategies, one for each player, we can simulate many
     * games, and establish who is winning on average.
     *
     * Q3. Compute  an  infinite  stream of  results,  Stream[Result],
     * given two  input strategies: 'alwaysScissors' for  Player 1 and
     * 'alternating' for Player 2.
     *
     * You can  reuse the  function 'winner' from  above, and  the two
     * streams defined in  Q2.  If you have not solved  Question Q1 or
     * Q2, then  just assume that  the definitions exist.  You  do not
     * need to run them to solve this question.
     */

    lazy val results: Stream[Result] = ???

    /**
     * Q4. Compute  the   fraction  how   many  times  the   Player  1
     * (alwaysScissors) wins on average  again Player 2 (alternating),
     * using a prefix of 500 entries.   You do not have to compute the
     * actual value. Just show the code that computes it.
     *
     * Reuse the  stream results from  Q3.  Assume that the  stream is
     * defined, if you have not solved Q3.
     */

    lazy val scissorsFraction: Double = ???

    /**
     * Now  we   change  from   simple  deterministic   strategies  to
     * simple  probabilistic  strategies,  so   instead  of  a  stream
     * (Stream[Move]),  a   strategy  is  a   distribution  indicating
     * what  is  the  probability  distribution  over  the  next  move
     * (Element[Move]).
     *
     * Q5. Create two strategies: Alice and Bob.
     *
     * a) Alice  picks  Rock,   Paper,  and  Scissors  with  uniform
     *    probability.
     *
     * b) Bob Picks  Rock and  Paper with  probability 0.5  each, and
     *    never picks Scissors.
     *
     * Links to relevant documentation (if you forgot from the course):
     *
     * https://www.cra.com/Figaro_ScalaDoc/com/cra/figaro/library/atomic/discrete/Uniform$.html
     * https://www.cra.com/Figaro_ScalaDoc/com/cra/figaro/language/Select$.html
     */

    type Strategy = Element[Move]

    lazy val Alice: Strategy = ???

    lazy val Bob: Strategy = ???

    /**
     * Q6. Implement  a function  that  computes a  result  of a  game
     * given two  probabilistic strategies. This function will  give a
     * probability of winning for each  player, and a probability of a
     * draw.
     *
     * Again, assume  that the function 'winner'  defining the outcome
     * is available, even if you have not solved Q1.
     */

    def game (player1: Strategy, player2: Strategy): Element[Result] = ???

    /**
     * Q7. Use importance  sampling to  estimate the  probability that
     * Alice is winning when playing against  Bob.  You do not have to
     * report the number,  just show the Scala code  that computes the
     * probability.
     *
     * Link to scaladoc (if you forgot the details form the course):
     *
     * https://www.cra.com/Figaro_ScalaDoc/com/cra/figaro/algorithm/sampling/Importance.html
     */

    lazy val aliceFraction: Double = ???

  }



  object MonadsAndTesting {

    /**
     * Part 2. MONADS AND TESTING
     *
     * We introduce a simple Monad, called Trivial.  The Trivial monad
     * adds unit and  flatMap to *any* data  type.  The implementation
     * is given below.
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
     * Q8. Explain why flatMap is  implemented twice above? Write your
     * answer below  in a  comment in English.   Expected length: 5-10
     * lines.  Lines have  been numbered in the program  above to help
     * you refer to them unambiguously, if you wanted to.
     */

    // ...

    /**
     * We shall now be testing whether  a Trivial is actually a monad.
     * A property  testing class  is set up  below. And the  first law
     * written is the identity law.
     */

    class TrivialSpec extends FreeSpec with PropertyChecks with Matchers {

      "Trivial Monad is a monad" - {

        // "Trivial monad satisfies the identity law" in {
        //   forAll { (tmx: Trivial[Int]) =>
        //     Trivial.map (tmx) (identity) shouldBe tmx
        //  }
        // }

        /**
         * Q9. The test  above is commented  out, because it  fails to
         * compile with the following error message:
         *
         * could not find implicit value for parameter arbA:
         * org.scalacheck.Arbitrary[adpro.Exam2019AutumnSolution.MonadsAndTesting.Trivial[Int]]
         *
         *   forAll { (tmx: Trivial[Int]) => Trivial.map (tmx) (identity) shouldBe tmx }
         *          ^
         * Implement the  missing piece of  code that fixes  the error
         * message (namely show evidence that an instance of Arbitrary
         * for  Trivial[Int]  exists). For   best  grade,  write  this
         * evidence  for Trivial[A]  and  make the  compiler find  the
         * evidence for Arbitrary[Trivial[Int]] by itself.
         */

        // Write here ...

        /**
         * Q10. A monad must also satisfy the associativity law. Write
         * a  property test  that checks  that Trivial[Int]  is indeed
         * associative   (using  the   standard   definition  of   the
         * associative law for Monads).
         */

        "Trivial monad is associative" in {

          // ...

        }

        /**
         * Q11. Use a  suitable mechanism in Scala  to inject .map and
         * .flatMap  as extension methods  for any type A, so that any
         * type  can be seen as a trivial monad.
         *
         * Hint: So far,  we can  construct an instance  of Trivial[A]
         * for any  concrete value a  by calling Trivial(a).   Then we
         * can call map and flatMap on this instance:
         *
         *    Trivial(a).map _
         *    Trivial(a).flatMap _
         *
         * (like in the tests above)
         *
         * It   is   slightly  annoying   that   we   need  to   write
         * 'Trivial(a).map'   instead  of   just  'a.map'. After   the
         * exercise is solved we should be able to write the following
         * for any type A and a value 'a' of type A.
         *
         *   a.map _
         *   a.flatMap _
         *
         * The meaning of the above calls should be the same as in the
         * monad  Trivial[A], and  the  solution  should work  without
         * modifying the type A (A is a generic type parameter).
         */

        // Write below ...

        /**
         * Q12. Add support  for implicit unwrapping of  Trivial monad
         * values so that the expressions below are type correct:
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

        // Write below ...

      }

    }

  }


  object Lenses {

  /**
   * Part 3. LENSES AND FINGER TREES
   *
   * (Skip this if you are  an MSEN Student. If this sounds mysterious
   * then do not skip.  Students  enrolled in MSc in Computer Science,
   * and in MSc  in Software Design, and single  subject students will
   * be graded on this questions)
   *
   * We are interested in creating a  lens for finger trees that would
   * allow to access the second (from the left) element in the tree.
   *
   * Q13. Will  such  a lens  be  total  or  partial? And will  it  be
   * then  modeled  as  an  Optional  or as  a  Lens  in  the  Monocle
   * library? Answer  and  explain  why.    Estimated  length  of  the
   * answer: 2-6 lines.
   */

  // Write here ...

  /**
   * Q14. Implement the above lens, peekLL.
   *
   * When the tree is  empty, and we need to set  a value through this
   * lens, we need  to invent the first element as  well. Just use the
   * same  element  twice,  both  for the  first  left-most,  and  the
   * second-leftmost position.
   */

   def peekLL[A] = {

     def get (t: FingerTree[A]) = ???

     def set (a: A) (t: FingerTree[A]) = ???

     // Uncomment one  of the  following lines,  and delete  the other
     // one, based on your answer to Q13:

     // Optional (get) (set)
     // Lens (get) (set)
   }


   /**
    * Q15. For  your  solution  to  Q14 explain  in  English  if  your
    * implementation  of  peekLL  satisfies the  GetPut  (GetSet)  and
    * PutGet (SetGet) laws.
    *
    * For the  answer to this  question, assume that two  finger trees
    * are equal if they store the  same elements in the same order, so
    * our notion of equality checks for trees t1 and t1 whether
    *
    *      t1.toList == t2.toList
    *
    * Estimated size of the answer 10-20 lines.
    */

   // Write here ...

  }
}
