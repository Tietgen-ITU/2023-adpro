/* These tests have not been available for the students writing the exam. */
package adpro.solution

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.{forAll, forAllNoShrink}

import fpinscala.answers.state.RNG
import fpinscala.answers.laziness.LazyList
import fpinscala.answers.laziness.LazyList.*

object Exam2022AutumnSpec
  extends org.scalacheck.Properties("exam-2022")
