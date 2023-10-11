// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.par

import java.util.concurrent.{Executors, ExecutorService, Callable}

import scala.language.implicitConversions
import scala.io.Source


/* This non-blocking  version of Future (different from
 * java.util.concurrent) takes a continuation instead of returning a
 * value, and calls it when ready.
 */
opaque type Future[+A] = (A => Unit) => Unit

opaque type Par[A] = ExecutorService => Future[A]



/** This call, extracting the value will block the current thread on
  * latch.await. The users of API should not call it earlier than
  * needed. Run should be called as late as possible, and programming
  * should proceed using the other APIs to allow high concurrency (not
  * wasting a thread).
  */
extension [A] (pa: Par[A])

  def run(es: ExecutorService): A =
    val ref = java.util.concurrent.atomic.AtomicReference[A] ()
    val latch = java.util.concurrent.CountDownLatch(1) 
    pa (es) { a => ref.set (a); latch.countDown } 
    latch.await 
    ref.get

  def map2[B, C](pb: Par[B])(f: (A, B) => C): Par[C] =
    es => k => 
      var ar: Option[A] = None
      var br: Option[B] = None
      val combiner = Actor[Either[A, B]] (es) {
        case Left (a) =>
          if br.isDefined then Par.eval(es)(k(f(a,br.get)))
          else ar = Some(a)
        case Right(b) =>
          if ar.isDefined then Par.eval(es)(k(f(ar.get,b)))
          else br = Some(b)
      }
      pa(es) { a => combiner ! Left (a)  }
      pb(es) { b => combiner ! Right (b) }

  def map[B](f: A => B): Par[B] =
    pa.map2(Par.unit (())) { (a, _) => f(a) }

  // Scroll below to Exercise 1, in object Par
  // Come back here after solving Exercise 7
  
  // Exercise 8

  def chooser[B](f: A => Par[B]): Par[B] = 
    es => k => Par.eval (es) (f(pa.run(es)) (es) (k))
  // Exercise 9 continues in the Par object, in the bottom of the file



object Par:

  /** Pass the value to the continuation, the executor service is not used. 
    * Note that unit is strict, so `a` will be computed in the current thread!
    */
  def unit[A](a: A): Par[A] =
    es => k => k (a)
  
  /** A non-strict version of `unit`, which may allow for parallelism. */
  def delay[A](a: => A): Par[A] =
    es => k => k (a)
  
  def fork[A](a: => Par[A]): Par[A] =
    es => k => eval (es) (a (es) (k))
  
  /** A helper function, for evaluating an action asynchronously, using
    * the given `ExecutorService`. This is just to avoid having to
    * instantiate a new Callable object every time we want to evaluate
    * something in this file. Taking an argument by-name is much
    * idomatic to scala (and lazy programming) than creating callable
    * objects (Java-style).  The ExecutorService is a Java API so we
    * create a small adaptation here to make things nice.
    */
  def eval(es: ExecutorService) (r: => Unit): Unit =
    es.submit (new Callable[Unit] { def call = r })
  
  def lazyUnit[A](a: => A): Par[A] = 
    fork(unit(a))

  // Exercise 1
  
  /* Write the answer here in a comment 
   * The reason is that it will wait until the Par type is actually being 
   * executed. So it uses by name so that it can wait computing the potentially
   * heavy computation.
   * The reason is that it will wait until the Par type is actually being 
   * executed. So it uses by name so that it can wait computing the potentially
   * heavy computation.
   */
  
  // Exercise 2 
  
  def asyncF[A, B](f: A => B): A => Par[B] = 
    a => lazyUnit(f(a))
  
  // Exercise 3
  /* Write the answer here in a comment:
   * I would first test that the map didn't execute anything, that makes it waiting. We can do this
   * by creating a function that takes a long time, and then time how long it takes map to return,
   * the new Par[B]. It should just build our Par structure, so it should be instantly. 
   * Secondly I would ensure that the mapping function works. We can do that by providing a 
   * unit(Int) and a mapping function would turn the Int into a string. We would then call run and see
   * if it returns the valu we expected.
   */
  
  // Exercise 4

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight[Par[List[A]]](unit(Nil))((a,b) => a.map2(b)((x,y) => x::y))

  /* This is shown in the book: */
  def parMap[A, B](as: List[A]) (f: A => B): Par[List[B]] =
    sequence(as.map (asyncF (f)))

  // Exercise 5

  def wget(uris: String*): List[String] =
    val es = java.util.concurrent.Executors.newFixedThreadPool(4)
    val res = parMap(uris.toList)(uri => scala.io.Source.fromURL(uri)("ISO-8859-1").mkString).run(es)
    es.shutdown()
    res

  /* Write your explanation in English here:
   * This runs in parallel because we use the previous parMap, that given a
   * list of items, maps items in parallel, by using the asyncf 
   */

  // Exercise 6

  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] =

    def g(a:A): Option[A] =
      if f(a) then
        Some(a)
      else None

    as.map(asyncF(g)).foldRight[Par[List[A]]](unit(Nil))((x,y) => x.map2(y)((a,b) => a match
      case None => b
      case Some(value) => value::b
    ))

  // Exercise 7

  // Note: The solution from the text book does not apply immediately.
  // It has to be adapted to the non-blocking representation of Par and
  // Future that we are using in this chapter.
  def choiceN[A](pn: Par[Int])(choices: List[Par[A]]): Par[A] =
    pn.map2(sequence(choices))((idx, ls) => ls(idx))
 
  def choice[A](pb: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] =
    def btoi(pb: Par[Boolean]): Par[Int] =
      pb.map(x => if x then 0 else 1)

    choiceN(btoi(pb))(t::f::Nil)

  // Exercise 8 is found above, in the extension methods of Par[A]
  // Come back here when done with Exercise 8.

  // Exercise 9
 
  def join[A](p: Par[Par[A]]): Par[A] =
    p.chooser(x => x)
 
  def chooser[A, B](p: Par[A])(f: A => Par[B]): Par[B] =
    join(p.map(x => f(x)))

end Par




// Exercise 10

// The types here are broken, and you need to fix them
// replace THIS with a meaningful name
extension [A] (papa: Par[Par[A]]) 
  def join: Par[A] = 
    Par.join(papa)

// The types here are broken, and you need to fix them
extension (pa: Par[Int]) 
  def choiceN[A] (pb: List[Par[A]]): Par[A] = 
    Par.choiceN(pa)(pb)

// The types here are broken, and you need to fix them
extension (pa: Par[Boolean]) 
  def choice[A] (f: Par[A], g: Par[A]) = 
      Par.choice(pa)(f, g)

/* Write your answer in English here:
 * This is due to the different type parameter which 
 * we cannot guarantee a common supertype for
 */
