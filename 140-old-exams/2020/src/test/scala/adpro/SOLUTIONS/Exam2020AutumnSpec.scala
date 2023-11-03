package adpro.SOLUTIONS

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

import adpro._

class Exam2020AutumnSpec
  extends org.scalatest.freespec.AnyFreeSpec
  with org.scalatest.Inside
  with org.scalatest.matchers.should.Matchers
  with org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
  with org.scalatest.prop.TableDrivenPropertyChecks {

  "Part 1" - {

    "A test that always passes (a sanity check)" in {
      forAll { (n: Int) => n shouldBe n }
    }

  }


  "Q1 (smashOption)" - {

    "Several simple test cases" in {
      Q1.smashOption (List (1,2,3,4)) should
        be (Some (List (1 -> 2, 3 -> 4)))
      Q1.smashOption (List ()) should
        be (Some (Nil))
      Q1.smashOption (List (42)) should
        be (None)
    }

  }

  "Q2 (smash splittable)" - {

    implicit val splitDouble = new Q2.Splittable[Double] {
      def split (x: Double) = .5*x -> .5*x
    }

    implicit val splitInt = new Q2.Splittable[Int] {
      def split (n: Int) = n/2 -> n/2
    }

    "Several simple test cases" in {
      Q2.smash (List (1,2,3,4)) should
        be (List (1 -> 2, 3 -> 4))
      Q2.smash (List[Double] ()) should
        be (Nil)
      Q2.smash (List (43)) should
        be (List (21 -> 21))
      Q2.smash (List[Double] (1,2,3)) should
        be (List (1 -> 2, 1.5 -> 1.5))
    }
  }

  "Q3 (splittable instances)" - {

    "Several simple test cases" in {

      Q3.splitDouble.split (43.0) should
        be (21.5 -> 21.5)
      Q3.splitListInt.split (List (1,2,3,4,5,6,7)) should
        be (List (1,3,5,7) -> List (2,4,6))


    }

  }

  "Q5 (successes)" - {

    "Several simple test cases" in {

      Q5.successes (Stream (Right (42))).toList should
        be (List (42))

      Q5.successes (Stream (Left (42), Right (42), Left (42))).toList should
        be (List (42))

      Q5.successes (Stream ()).toList should
        be (List ())

    }

  }

  "Q7 (vector)" - {

    import Q7._

    "Failed vector" in {
      val v0: VectorD = None
      forAll { n: Int => v0 (n) should be (None) }
    }

    "Non-failed vector 1,2,3" in {

      val v1: VectorD = Some (List (1,2,3))
      forAll { n: Int =>
        val n1 = Math.abs (n % 10000)
        if (n1 <  3)
          v1 (n1) should be (Some (n1+1))
        else
          v1 (n1) should be (None)
      }
    }
  }


  "Q8 (polymorphic recursion)" - {

    import Q8._

    "regressions" in {
      flattenBox (box) should
        be (List (1,2,3,4,5,1,2,3,4,42,42,42,42))

      flattenBox (b) should
        be (List (List (2,5), List (1), List (3,4), List (42,42,42)))
    }
  }


  "Q9 (parsing CSV)" - {

    import adpro.Parsing._
    import adpro.Parsing.MyParsers.string
    import adpro.Parsing.MyParsers.operators
    import adpro.Parsing.MyParsers.{ fail => FAIL }

    "regressions" in {
      adpro.Parsing.MyParsers.run (Q9.NL) ("\n") should
        be (Right ("\n"))
      adpro.Parsing.MyParsers.run (Q9.NL) (" \n") should
        matchPattern { case Left (_) => }
      adpro.Parsing.MyParsers.run (Q9.commaSeparatedInts) ("42") should
        be (Right (List (42)))
      adpro.Parsing.MyParsers.run (Q9.commaSeparatedInts) ("42\n") should
        be (Right (List (42)))
      adpro.Parsing.MyParsers.run (Q9.parser) ("") should
        be (Right (List()))
      adpro.Parsing.MyParsers.run (Q9.commaSeparatedInts) ("0\n0") should
        be (Right (List(0)))
      adpro.Parsing.MyParsers.run (Q9.NL |* Q9.commaSeparatedInts) ("\n0") should
        be (Right (List(0)))
      adpro.Parsing.MyParsers.run (Q9.csvInt) ("0\n0") should
        be (Right (List (List(0), List(0))))
      adpro.Parsing.MyParsers.run (Q9.parser) ("0\n0") should
        be (Right (List (List(0), List(0))))
    }

    "let's generate and parse csv lines!" in {

      forAll { l: List[Int] =>
        whenever (!l.isEmpty) {

          val input = l.mkString (",")
          val result = adpro.Parsing.MyParsers.run (Q9.commaSeparatedInts) (input)

          inside (result) {

            case Right (outcome) =>
              outcome should be (l)

            case Left (msg) =>
              fail (msg.toString)
          }
        }
      }
    }

    "let's generate and parse csv files!" in {

      forAll { l: List[List[Int]] =>
        whenever (l.forall { !_.isEmpty }) {

          val input = l
            .map { _.mkString (",") }
            .mkString ("\n")

          // info ("#" + l.toString + "#" + input + "#")

          val result = adpro.Parsing.MyParsers.run (Q9.parser) (input)

          inside (result) {

            case Right (outcome) =>
              outcome should be (l)

            case Left (msg) =>
              fail (msg.toString)
          }
        }
      }
    }
  }

}


class Q4SmashSpec extends Q4.SmashSpec
