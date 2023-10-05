// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.par

import org.scalacheck.*
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.*
import java.util.concurrent.ExecutorService

/* Increase here if tests are failing for no apparent reasons
 * Testing concurrency in a hardware-independent manner is not easy.
 */
val TIMEOUT: Int = 30 // miliseconds

def pool(n: Int) = 
  java.util.concurrent.Executors.newFixedThreadPool(n)

def runAndShutDown[A] (test: Par[A]) (es: ExecutorService): A =
  val result = test.run (es)
  es.shutdown ()
  result

val parList: List[Par[Int]] = 
  List (0, 1, 2, 3)
    .map (Par.unit[Int])

val parCrash: Par[Nothing] = Par.lazyUnit (???)

/* Purposely imperative, we create data races to test! */
private class Gadget (var counter: Int = 0):  

  def task[A] (n: A): A =
    val result = counter + 1
    Thread.sleep (TIMEOUT)
    counter = result
    n
   
  def parAssert[A] (pa: Par[A]) (p: Int => Boolean): Par[Boolean] =
    val pTest = Par.lazyUnit (p (this.counter))
    pa.map2 (pTest) { (parResult, testResult) => testResult }

end Gadget


object ParSpec
  extends org.scalacheck.Properties("par"):
  
  // Exercise 2 (asyncF)

  property("Ex02.01: asyncF lazy (strict fails, non-strict passes even if sequential)") =
    val g = Gadget ()
    Par.asyncF (g.task) (42)
    { g.counter == 0 } :| s"g.counter == ${g.counter} (expected 0)"

  property("Ex02.02: asyncF parallel (fails if not parallel, even if lazy)") =
    val g = Gadget ()
    val t = g.parAssert (Par.asyncF (g.task) (42)) { _ == 0 }
    runAndShutDown (t) (pool (2))

  // Exercise 4 (sequence)

  property("Ex04.01: sequence does not force the computation") =
    Par.sequence (List (parCrash))
    true
  
  property("Ex04.02: sequence does not wait eagerly for completion") =
    val g = Gadget ()
    val l = List.fill (10) (Par.lazyUnit (g.task (42)))
    val t = g.parAssert (Par.sequence (l)) { _ < 10 }
    runAndShutDown (t) (pool (2))

  property("Ex04.03: sequence does not loose any elements") =
    val g = Gadget ()
    val l = List.fill (5) (Par.lazyUnit (g.task (42)))
    val t = g.parAssert (Par.sequence (l)) { _ == 5 }
    runAndShutDown (t) (pool (1))

  // The idea is that if there is some parallelism some tasks would have
  // started before their predecessors have incremented the counter.  This is
  // impossible if using a pool of a single thread (you can check).
  property("Ex04.04: sequence still runs elements in parallel") =
    val g = Gadget ()
    val l = List.tabulate (10) { i => 
      Par.lazyUnit { g.task (i); g.counter < i-1 } }
    runAndShutDown (Par.sequence (l)) (pool (2))
      .exists  (identity[Boolean])

  // The idea is that if there is some parallelism some tasks would have
  // started before their predecessors have incremented the counter.  This is
  // impossible if using a pool of a single thread (you can check).
  property("Ex04.05: parMap runs in parallel (based on sequence)") =
    val g = Gadget ()
    val l = List.tabulate[Int] (5) (identity) 
    val p = Par.parMap (l) { i => g.task (42); g.counter < i - 1 }
    runAndShutDown (p) (pool (5))
      .exists (identity[Boolean])

  // Exercise 5 (wget)

  // Will crash if the implementation crashes, but otherwise has 
  // no way to investigate parallelism
  property("Ex05.01: This is not a test, just run wget, print 100 chars of the first line from 3 sites") =
    for
       html <- Par.wget ("https://www.itu.dk", "https://www.google.com")
       head = html.split ("\n").head.take (100)
    yield println (head)
    true
    
  // Exercise 6 (parFilter)

  property("Ex06.01: parFilter does not force the computation") =
    Par.parFilter[Int] (List (1)) (n => ???)
    true

  property("Ex06.02: parFilter does not wait eagerly for completion") =
    val g = Gadget ()
    val l = List.fill (5) (42)
    val f = Par.parFilter (l) (_ => g.task (true))
    val t = g.parAssert (f) { _ < 5 }
    runAndShutDown (t) (pool (2))

  property("Ex06.03: parFilter tries all the elements") =
    val l = List.fill (5) (42)
    val t = Par.parFilter (l) { _ => true }
    runAndShutDown (t) (pool (2)) == l

  property("Ex06.04: parFilter runs the elements in parallel") =
    val g = Gadget ()
    val l = List.tabulate[Int] (5) (identity) 
    val p = Par.parFilter (l) { i => g.task (42); g.counter < i - 1 }
    runAndShutDown (p) (pool (4))
      .nonEmpty

  property("Ex06.05: parFilter is functionally equivalent to filter") =
    given Arbitrary[List[Int]] = 
      Arbitrary (Gen.listOfN (200, Gen.choose(0, 2000)))
    forAll { (l: List[Int], f: Int => Boolean) =>
      l.filter (f) == runAndShutDown (Par.parFilter (l) (f)) (pool (16)) }
 
  // Exercise 7 (choiceN, choice)

  property("Ex07.01: choiceN does not force the computation, just returns") =
    Par.choiceN (Par.unit (0)) (List (parCrash))
    true

  property("Ex07.02: choiceN returns the right computation") =
    forAll (Gen.oneOf (0, 1, 2, 3)) { (i: Int) => 
      val ch = Par.choiceN (Par.lazyUnit (i)) (parList)
      runAndShutDown (ch) (pool (2)) == i }

  property("Ex07.03: choice does not force the computation, just returns") =
    Par.choice (Par.unit (true)) (parCrash, parCrash)
    true

  property("Ex07.04: choice returns the right computation") =
    forAll { (b: Boolean) =>
      val pb = Par.choice (Par.lazyUnit (b)) (Par.lazyUnit (true), Par.lazyUnit (false))
      runAndShutDown (pb) (pool (2)) == b }
 
  // This test is on hold, because the book does not discuss speculative
  // execution. Perhaps we'll add it at some point
  // "choosing a fast computation in choice N is fast" in { }

  // This test is on hold, because the book does not discuss speculative
  // execution. Perhaps we'll add it at some point
  // "choice returns the fast computation faster" in {}
 
  // Exercise 8 (chooser)

  property("Ex08.01: chooser does not force the computation, just returns") =
    Par.unit (true).chooser { _ => parCrash }
    true

  property("Ex08.02: chooser returns the right computation") =
    forAll (Gen.nonEmptyListOf (Gen.alphaChar)) { l =>
      forAllNoShrink (Gen.choose (0, l.size - 1)) { i =>
        val ch = Par.lazyUnit (i).chooser  { a => Par.lazyUnit (l (a)) }
        runAndShutDown (ch) (pool (2)) == l (i) } }
 
  // Exercise 9 (join, chooser via join)

  property("Ex09.01: join joins") =
    forAll  { (n: Int) =>
      val j = Par.join (Par.lazyUnit (Par.lazyUnit (42)))
      runAndShutDown (j) (pool (5)) == 42 }

  property("Ex09.02: chooser (via join) does not force the computation, just returns") =
    Par.chooser (Par.unit (true)) { _ => parCrash }
    true

  property("Ex09.03: chooser (via join) returns the right computation") =
    forAll (Gen.nonEmptyListOf (Gen.alphaChar)) { l =>
      forAllNoShrink (Gen.choose (0, l.size - 1)) { i =>
        val ch = Par.chooser (Par.lazyUnit (i)) { a => Par.lazyUnit (l (a)) }
        runAndShutDown (ch) (pool (2)) == l (i) } }
 
  // Exercise 10 (extension methods)

  property("Ex10.01: extension method join joins") =
    forAll  { (n: Int) =>
      val j = Par.lazyUnit (Par.lazyUnit (42))
        .join
        .asInstanceOf[Par[Int]]
      runAndShutDown (j) (pool (5)) == 42 }

  property("Ex10.02: choiceN (method) does not force the computation, just returns") =
    Par.unit (0).choiceN (List (parCrash))
    true

  property("Ex10.03: choiceN (method) returns the right computation") =
    forAll (Gen.oneOf (0, 1, 2, 3)) { (i: Int) => 
      val ch = Par.lazyUnit (i).choiceN (parList).asInstanceOf[Par[Int]]
      runAndShutDown (ch) (pool (2)) == i }

  property("Ex10.04: choice (method) does not force the computation, just returns") =
    Par.unit (true).choice (parCrash, parCrash)
    true

  property("Ex10.05: choice (method) returns the right computation") =
    forAll { (b: Boolean) =>
      val pb = Par.lazyUnit (b)
        .choice (Par.lazyUnit (true), Par.lazyUnit (false))
        .asInstanceOf[Par[Boolean]]
      runAndShutDown (pb) (pool (2)) == b }
