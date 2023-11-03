/************************************************************************
 * Final Exam: Advanced Programming by Andrzej Wąsowski
 * IT University of Copenhagen, Autumn 2020: 6 January 2021, 9:00
 *
 * Your Full Name: ___
 * Your ITU email account: ___
 *
 * The exam consists of 9 questions to be solved within 4 hours.
 *
 * It is possible  to answer later questions, missing  the answers for
 * the previous ones, but it is recommended to answer in order.
 *
 * You can use  any function from the course  (textbook, exercises) in
 * the  solutions, as  well as  standard library  functions.  You  can
 * access any written or electronic material, also online, but you are
 * not allowed to communicate with anybody during the exam.
 *
 * By  submitting, you  declare  to have  solved  the problems  alone,
 * without communicating with anybody.
 *
 * SUBMISSION
 *
 * Solve the tasks in the file 'Exam2020Autumn.scala' (this file) found
 * in the zip archive made available on LearnIt.
 *
 * Fill in your name and your ITU email above, in the top of the file.
 *
 * Submit this file and only this  file to learnIT.  Do not convert to
 * any  other  format than  .scala.   Do  not  submit the  entire  zip
 * archive. Do not  reorder the  answers, and  do not  remove question
 * numbers from the file.  The only accepted file format is '.scala'.
 *
 * Keep the solutions within 80 columns width to make grading easier.
 *
 * ADVICE
 *
 * The  answers  will   be  graded  manually. We  will   focus  on  the
 * correctness of ideas and the use  of the course concepts. We will be
 * permissive on  minor issues  such as semicolons,  other punctuation,
 * small deviations  in function  names, switching between  curried and
 * not  curried arguments,  etc.  We  will not  check whether  the type
 * inference succeeds.   It suffices  that a  human reader  could infer
 * types.
 *
 * We do not recommend solving questions to the point when they compile
 * and pass tests.  Dependency problems  and other technical issues can
 * take a lot of time, so only do this, once you are done with drafting
 * all answers.
 *
 * Nevertheless, if  you do compile,  you can use the  'build.sbt' file
 * provided  in the  zip  archive linked  above. It  has the  necessary
 * library dependencies  configured. The zip archive also  contains the
 * course libraries that the solutions depend on.
 *
 * The  percentage  at  the   beginning  of  each  question  indicates
 * difficulty of the question, and will be used as guide in grading.
 *
 * Good luck!
 */

package adpro

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks._
import org.scalatest.matchers.should.Matchers._
import org.scalactic.Equality


object Q1 { // 10%

  /**
   * BASIC RECURSION, EXCEPTIONS, AND REFERENTIAL TRANSPARENCY
   *
   * The following function  'smash' makes a list  thicker and shorter
   * by turning into a list of  pairs.  It fails with an exception, if
   * the list it receives has an  odd number of elements, as it cannot
   * smash the last odd element.
   */

  def smash[A] (l: List[A]): List[(A,A)] =
    l match {
      case List () =>
        Nil

      case h1 :: h2 :: tl =>
        h1 -> h2 :: smash (tl)

      case _ =>
        throw new NoSuchElementException ()
    }

  /**
   * Examples :
   *
   * smash (List (1,2,3,4)).size should
   *   be (2)
   *
   * smash (List (1,2,3,4)) should
   *   be (List (1 -> 2, 3 -> 4))
   *
   * Write a function 'smashOption'  that is referentially transparent
   * and  achieves  the  corresponding functionality  purely,  in  the
   * Option monad.  Include the return type in the function signature:
   */

  def smashOption = ???

}



object Q2 { // 10%

    /**
     * TYPE CLASSES AND TYPE CONSTRAINTS
     *
     * Another way to  handle errors in smash is to  make the function
     * total.  This could be done by assuming that the element type is
     * 'splittable', so  that we can split  an element in half,  if we
     * are left with an odd one.
     *
     * For example, let's  imagine that a Float can  be split dividing
     * the  value  into  two.   Then  smash  could  be  producing  the
     * following:
     *
     *   smash (List[Double] (1,2,3)) == List (1 -> 2, 1.5 -> 1.5)
     *
     * For this we need to assume  that the type variable A represents
     * a splittable type.   Let's say that a type is  splittable if it
     * has an instance of the type class Splittable implemented by the
     * following trait:
     */

    trait Splittable[A] {
      def split (a: A): (A,A)
    }

    /**
     * Implement a new version of smash  that is total and smashes all
     * values of List[A] but requires that A is splittable
     */

    def smash = ???

  }



object Q3 { // 10%

  import Q2._

  /**
   * TYPE CLASS INSTANCES
   *
   * Implement two instances of the Splittable type class:
   *
   * - For Double, splitting a number into two, dividing by 2.0
   * - For List[Int], splittling a list by putting all elements on odd
   *   positions in the left list,  and all elements from even positions
   *   in the right list
   */

  // implicit val splitDouble = ...

  // implicit val splitListInt = ...

}



object Q4 { // 15%

  import Q2._
  import Q3._

  /**
   * PROPERTY-BASED TESTING
   *
   * We want to test your smash from Q2.  Write the following tests:
   *
   * - The size of a smashed list is half of the original list size
   *   before smashing, for any list with even number of elements
   *
   * - The size of the smashed list is the size of the original list
   *   divided by half (integer division) and increased by one, for
   *   lists with odd number of elements.
   *
   * Choose the concrete type to test on, so that you use one of the
   * instances created by you in Q3 above.
   *
   * If you did not  solve Q2 or Q3, write the  test for the example
   * implementation of smash from Q1.
   */

  class SmashSpec
    extends org.scalatest.freespec.AnyFreeSpec
    with org.scalatest.matchers.should.Matchers
    with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks {

    "Write the first test here" in {

      fail ()  // <-- replace with test
    }

    "Write the second test here" in {

      fail ()  // <-- replace with test
    }
  }
}



object Q5 { // 5%

  import adpro._

	/**
   * LAZY STREAMS
   *
   * Consider a (possibly infinite) lazy stream of Either[A,B] values,
   * where  the left  component represents  a failure,  and the  right
   * component represents  a success.   Assume the  following liveness
   * property: there is always a success after several failures in the
   * stream. Convert  this stream  into  a stream  of successes,  that
   * maintains its laziness.
	 *
   * You can use  any function of the  book implementation
   * of streams.
	 */

	def successes[A,B] (results: Stream[Either[A,B]]): Stream[B] = ???

}



object Q6 { // 10%

	/**
   * REFLECTION ON LAZY STREAMS
   *
   * Explain what  problems could  happen if  the stream  of 'results'
   * above does not guarantee liveness,  i.e. there could be a success
   * after which there are infinitely  many failures.  Explain how the
   * computation of 'successes' fails  (how the problem may manifest)?
   * Explain what  is the fault  (what mechanism causes  the failure)?
   * The indicative size of the answer is 5-10 lines, more and less is
   * allowed.
   *
   * ???
   */

}



object Q7 { // 10%

  /**
   * EXTENSION METHODS / PIMP MY LIBRARY PATTERN
   *
   * We are  interested in  implementing a type  of vectors  of double
   * numbers. We decided that we will represent these vectors as lists
   * of doubles wrapped  in an option, so that we  can capture failure
   * of vector operations:
   */

  type VectorD = Option[List[Double]]

  /** For instance (read quickly, this is just an example): */

  def vector_plus2 (v1: VectorD, v2: VectorD): VectorD =
    for {
      l1 <- v1
      l2 <- v2
      if l1.size == l2.size
    } yield (l1 zip l2) map { case (x,y) => x + y }

  /**
   * Use the pimp-my-library pattern to add support for element access
   * to vectors, so that  if 'v' is a VectorD we can  write 'v (0)' to
   * access the first element, etc.   The access 'v (i)' should return
   * a value  of type  Option[Double]. It should be  'Some' containing
   * the value stored  at the ith position, or 'None'  if the index is
   * out of bounds.
   */

  ???

}



object Q8 { // 10%

  /**
   * POLYMORPHIC RECURSION
   *
   * Consider  the following  type  Box  that defines  polymorphically
   * recursive lists. In Boxes of type A we  have a head of type A and
   * a tail of type Box[List[A]].  Note that the question has 3 parts.
   */

  sealed trait Box[+A]

  case class Pack[A] (
    hd: A,
    tl: Box[List[A]]
  ) extends Box[A]

  case object End
  extends Box[Nothing]

  /**
   * An example value, storing these numbers in order:
   * 1, 2, 3, 4, 5, 1, 2, 3, 4, 42, 42, 42, 42
   */
  val box: Box[Int]  =
    Pack(
      1,
      Pack (
        List (2,3,4,5),
        Pack (
          List (List (1), List(2,3,4), List (42,42,42,42)),
          End
        )
      )
    )

  /**
   * 1. Write a polymorphic function flattenBox  that, for any type A,
   * converts  a Box[A]  value into  a  usual Scala  List[A] value  by
   * traversing the elements in order.
   */

  def flattenBox = ???

  /**
   * 2. Which call in your solution is polymorphically recursive?  Add
   *    a type  annotation on the polymorphically  recursive call, and
   *    explain  in  English  below  which  call  it  is  and  why  it
   *    is  polymorphically recursive  (how  can you  see  that it  is
   *    polymorphically recursive).  Expected size 4-5 lines (more and
   *    less is allowed)
   *
   *    ???
   */

  /**
   * 3. Consider the Box 'b' below. What is the result of running flattenBox on
   *    it? Write the answer and a short explanation in English why.
   *
   *    ???
   */

  val b = Pack (
        List (2,5),
        Pack (
          List (List (1), List(3,4), List (42,42,42)),
          End
        ))
}



object Q9 { // 15%

  import adpro.Parsing._
  import adpro.Parsing.MyParsers._

  /**
   * Use the parser  combinator library from the textbook  to write a
   * parser that  parses CSV files  of integer numbers (and  no other
   * types) The type produced should be List[List[Int]].
   *
   * Example input:
   *
   * 1,2,3 , 5,4
   * 42,42
   *
   * Each line  in the  file contains  integers separated  with commas
   * (',') and possibly  white space (tab or  space character).  Lines
   * are  separated  by  '\n'.   The  last line  may  or  may  not  be
   * terminated by '\n'.   There can be no empty lines  (except for in
   * the end  of the  file, if  the last line  is terminated  with the
   * optional \n character).   An empty file parses to an empty list.
   */

  lazy val parser = ???

}
