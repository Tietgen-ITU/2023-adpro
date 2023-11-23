// wasowski, Advanced Programming, IT University of Copenhagen
// change package to adpro.SOLUTIONS to test teacher's solutions
package adpro.prob

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, forAllNoShrink}
import org.scalacheck.Arbitrary.arbitrary

// Used by samplers below
given rng: spire.random.rng.SecureJava 
  = spire.random.rng.SecureJava.apply

import pigaro.*

object ProbSpec
  extends org.scalacheck.Properties("prob"):

  val Err: Double = 0.03

  /** Sample size for tests */
  val N = 4000 

  // n is the number of black balls (not the total number of balls)
  // returns the probability of the player making a move winning with
  // n black balls in the urn
  def analyticalP1(n: Int): Double =
    if n % 2 == 0 
    then (1 + n/2) / (n + 1.0) // see notes below
    else 0.5

  // n = 3 (#black)
  // 1/4 + 1/2 * (3/4*2/3) = 1/4 + 1/4 = 1/2
  // 1/3 * 3/4 + (3/4*2/3*1/2) = 1/4 + 1/4 = 1/2

  // n = 4 (#black)
  // 1/5 + (4/5 * 3/4) * 1/3 + (4/5 * 3/4 * 1/3 * 1/2) = 3/5
  // 1/4 * 4/5 + (4/5 * 3/4 * 2/3) * 1/2 = 1/5 + 1/5 = 2/5
  //
  //  (1 + n div 2) / (n+1) for the first player
  //  (n div 2) / (n+1)     for the second player

  // Exercise 6

  property(s"Ex06.01: blackBallsNo uniform in [0,$UpperBound)") = 
    val sample = blackBallsNo.sample(N)
    forAll (Gen.choose(0, UpperBound-1)) { (i: Int) =>
      forAll (Gen.choose(0, UpperBound-1)) { (j: Int) =>
        val pi = sample.pr(i)
        val pj = sample.pr(j)
        s"p($i=$pi p($j)=$pj" |: (pi - pj).abs <= Err
      }
    }

  property("Ex06.02: posterior of an odd urn after Player1 wins > 1/2") = 
    println (posteriorOdd)
    s"Prob. that we had an odd-sized urn: $posteriorOdd" |: 
      posteriorOdd > 0.5  

  // Exercise 5

  property(s"Ex05.01: Pr. Paula started given she won, w/ ${BallsNo} balls") =
     if BallsNo % 2 == 0 
     then (probPaulaStarted - 0.5).abs <= Err
     else probPaulaStarted > 0.5

  property("Ex05.02: gameWonByPaula conditioned on Paula's win = 1.0") =
    gameWonByPaula._2.sample(N).pr(Player.Paula) == 1.0 

  property(s"Ex05.03: gameResult with $BallsNo balls & uniform prior is 1/2") =
    val p = gameResult._2.sample(N).pr(Player.Paula)
    (p - 0.5).abs <= Err

  // Exercise 3

  property(s"Ex03.01 probPaulaStarts with $BallsNo balls in the urn") =
    val expected = analyticalP1(BallsNo - 1)
    (probPaulaStarts - expected).abs <= Err

  property(s"Ex03.02 probPeterStarts with $BallsNo balls in the urn") =
    val expected = 1.0 - analyticalP1(BallsNo - 1)
    (probPeterStarts - expected).abs <= Err


  // Exercise 2

  property("Ex02.01 move") =
    forAll (Gen.oneOf(0 to 7)) { (n: Int) =>
      val p1 = move(Player.Peter, n).sample(N).pr(Player.Paula)
      val p2 = move(Player.Paula, n).sample(N).pr(Player.Paula)
      { (p1 - (1.0 - analyticalP1(n))).abs <= Err }:| s"p1=$p1 is incorrect."
        && { (p2 - (analyticalP1(n))).abs <= Err } :| s"p2=$p2 is incorrect."
    }

  // Exercise 1

  property("Ex01.01 pick") = 
    forAll(Gen.choose(0, 10)){ (nBalls: Int) =>
      val expected = 1.0 / (nBalls + 1.0)
      val p = pick(nBalls).sample(N).pr(Ball.Red)
      (p - expected).abs <= Err
    }
