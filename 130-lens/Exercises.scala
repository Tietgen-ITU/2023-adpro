/* Advanced Programming, A. Wąsowski, IT University of Copenhagen 
 *
 * Monocle is a library providing lenses for Scala (one of many in fact)
 * 
 * A Tutorial and other documentation for Monocle lenses is here:
 * https://www.optics.dev/Monocle/
 *
 * We will reimplement some Lenses today, but we shall reuse some basic
 * infrastructure from Monocle.  Monocle is *probably* the most popular Lens
 * framework for scala, although scalaz provides its own lenses, and so does
 * shapeless.
 *
 * Some examples can be found here (and in other files in the same directory):
 *
 * https://github.com/optics-dev/Monocle/blob/9cfdb174cc55be3d8953243c192c141362cb9f38/example/src/test/scala-2/monocle/LensExample.scala *
 * (using Scala 2)
 *
 * Work through the file below in the order of numbered exercises (top to
 * bottom), referring to LensesSpec.scala whenever necessary.
 *
 * Some notes to keep in mind when working and reading:
 *
 * 1. Put is called replace in Monocle
 *
 * 2. Replace is curried (Morries [2012] has a discussion that touches on
 *    advantages of currying set)
 *
 * 3. Total lenses are called Lens in monocle.  Partial lenses are of type
 *    Optional.
 */

package adpro.lens

import monocle.*
import monocle.syntax.all.*
import monocle.function.all.*
import monocle.PLens.lensChoice

import org.scalacheck.{Arbitrary, Prop}
import org.scalacheck.Prop.*

import org.scalactic.Equality
import org.scalactic.TripleEquals.*

// Used by 'each' in monocle
import cats.Order
import cats.implicits.catsKernelStdOrderForString
import adpro.lens.Laws.putGet
import adpro.lens.Laws.putPut
import adpro.lens.Example4.index



/* Exercise 1
 *
 * Study the implementation of lens L1 below and compare it to the first
 * example in Foster et al. (Page 6).
 */

val L1 = Lens[(String,Int), String](get = _._1)(replace = s1 => _ => (s1, 0))

/* Now, complete the second example from page 6, and the example from page 7
 * below:
 */

lazy val L2: Lens[String, (String, Int)] = Lens[String, (String, Int)]
  (get = s => (s, 0))
  (replace = s1 => _ => s1._1)

/* Finally, complete the example from page 7 in Foster et al.:
 */

lazy val L3: Lens[(String,Int), String] =
  Lens[(String, Int), String]
    (get = _._1)
    (replace = s => s1 => if s == s1._1 then s1 else (s, s1._2+1))

/* We will test these implementations in Exercise 3.  For now, we are
 * satisfied if they type check and compile.
 */



/* Exercise 2
 *
 * Write the PutGet law as a property test for arbitrary lenses from type C to
 * type A. See further sub-questions below.
 */

object Laws:

  def putGet[C: Arbitrary, A: Arbitrary: Equality](l: Lens[C, A]): Prop =
    forAll((concrete: C, a: A) => l.get(l.replace(a)(concrete)) == a)

  /* Write the GetPut law: */

  def getPut[C: Arbitrary: Equality, A](l: Lens[C, A]): Prop =
    forAll((concrete: C) => l.replace(l.get(concrete))(concrete) == concrete)

  /* Write the PutPut law: */

  def putPut[C: Arbitrary: Equality, A: Arbitrary](l: Lens[C, A]): Prop =
    forAll((c: C, a1: A, a2: A) => l.replace(a1)(l.replace(a2)(c)) == l.replace(a1)(c))

  /* There is no tests for this exercise, but we will use the laws below, 
   * which will also test them.
   *
   * The two laws below may be useful in Exercise 3 (cf. Foster et al.).
   */

  def wellBehavedTotalLense[A: Arbitrary, C: Arbitrary](l: Lens[C, A]): Prop =
    (putGet(l) :| "PutGet law: ") && (getPut(l) :| "GetPut law: ")

  def veryWellBehavedTotalLense[A: Arbitrary, C: Arbitrary] (l: Lens[C,A]) =
    (wellBehavedTotalLense(l) :| "Well-behaved total lense") 
      && (putPut(l) :| "PutPut law")



/* Exercise 3 
 *
 * Test  lenses L1, L2, and L3 using the laws implemented above. Check
 * with the paper whether the results are consistent.
 *
 * Lens L1 fails getPut (see Foster). Check to convince yourself that it fails
 * and comment out or negate the getPut test before submission so that the test passes. 
 *
 * Lens L2 fails putGet see p. 6 in Foster (check to convince yourself that it
 * fails, and submit only the getPut and putPut test; comment out the putGet or negate it).
 *
 * Lens L3 fails PutPut (check yourself and submit only the tests for the other
 * laws of well behaved, like above)
 */ 

object Ex03Spec 
  extends org.scalacheck.Properties("ex03.."):

  property("Ex03.00: putGet && putPut for L1") =
    (Laws.putGet(L1) :| "PutGet Law") && (Laws.putPut(L1) :| "PutPut Law")

  property("Ex03.01: getPut && putPut laws") =
    (Laws.getPut(L2) :| "GetPut Law") && (Laws.putPut(L2) :| "PutPut Law")

  property("Ex03.02: L3 is well behaved") =
    Laws.wellBehavedTotalLense(L3)

end Ex03Spec 



/* Exercise 4
 * 
 * Implement the lens codiag from Either[A, A] to A as presented by [Morris,
 * 2012]. This may be thought of as taking either A or A and stripping the
 * choice of A from the Either value. The type of this value is Lens[Either[A,
 * A], A].
 *
 * (ca 10 lines of code)
 */

def codiag[A]: Lens[Either[A, A], A] =
  Lens[Either[A,A], A]
    (get = e => e match
      case Left(vl)  => vl
      case Right(vr) => vr
    )
    (replace = a => b => b match
      case Left(_) => Left(a) 
      case Right(_) => Right(a)
    )



/* Exercise 5
 *
 * Section 5.3 in Morris's paper [Morris  2012] describes a choice combinator
 * for lenseis |||: Lens[R, F] => Lens[S, F] => Lens[Either[R, S], F].
 *
 * Morris uses it to implement the above codiag lense together with an
 * identity lense (Identity is described in [Foster et al. p 12] and in
 * [Morris p.3 Listing 12].
 *
 * In Monocle '|||' is called "lensChoice.choice" and identity is called
 * "Iso.id", the same way as Morris calls it. Observe also that Monocle's
 * setters (replace) are curried, while Morris's are not.  Iso is a kind of
 * lens in Monocle, that looses no information --- it can be applied both ways
 * without information loss (isomorphism / bijection).
 *
 * Translate Morris's implementation of codiag to Monocle.
 *
 * (ca. 1 line of code)
 */

def codiag1[A]: Lens[Either[A, A], A] =
  lensChoice.choice(Iso.id, Iso.id)

/* Exercise 6
 *
 * Important: This exercise shows the main application of lenses
 * Consider the following types:
 */

type ZipCode = String
type Name = String
type Students = Map[Name, Address]

case class Address (
  val zipcode: ZipCode,
  val country: String
)

case class University (
  val students: Students,
  val address: Address
)

val itu = University(
  students = Map[Name, Address] (
    "Stefan"    -> Address ("2300",   "Denmark"),
    "Axel"      -> Address ("91000",  "France"),
    "Alex"      -> Address ("2800",   "Denmark"),
    "Christian" -> Address ("D-4242", "Germany"),
    "Andrzej"   -> Address ("00-950", "Poland"),
    "Thorsten"  -> Address ("6767",   "Sweden")
  ),
  address = Address("2300", "Amager")
)

/* Write an expression that modifies "itu" in such a way that Alex is in
 * Denmark but at post-code 9100. First do it *without* using lenses.
 * 
 * Hint: every class in Scala has a method called 'copy' that takes the same
 * parameters as the constructor.  All parameters are optional.  Use the name
 * assignment convention to only change values of properties that you want in
 * the copy.  For instance itu.copy(students = itu.students.tail) creates a
 * copy of ITU without the first student.
 * 
 * Reflect how inconvenient it is, even with the copy method. Notice, how easy
 * and natural this change would be in an imperative style (for instance in C#
 * or Java).
 */

lazy val itu1: University = 
  itu.students.get("Alex")
              .map(addr => itu.copy(students = itu.students + ("Alex" -> addr.copy(zipcode = "9100"))))
              .get
 
/* As you see doing this without lenses is very very annoying.  Updating
 * nested properties in complex objects is much easier in imperative
 * programming.
 * 
 * My Reflections:
 * It is very inconvenient to do this out selfes. Especially thinking about the amount of code that is necessary to
 * write every time we would have to "modify" a tree-like object.
 */ 


/* Exercise 7
 *
 * Lenses to the rescue.  Extend our hypothetical university library with
 * lenses, so that using the types is almost as natural as in imperative
 * languages.
 *
 * a) design a lense that accesses zipcode from Address objects: (1-2 lines)
 */

lazy val _zipcode: Lens[Address, ZipCode] =
  Lens[Address, ZipCode] (get = a => a.zipcode) (replace = z => a => a.copy(zipcode = z))

/* b) design a lense that accesses the students collection from university:
 *  (1-2 lines)
 */ 

lazy val _students: Lens[University, Students] =
  Lens[University, Students] (get = a => a.students) (replace = s => a => a.copy(students = s ))

/* c) Use the following partial lense 'index(name)' from Monocle:
 *
 * index(name): Optional[Map[String,Address], Address]
 *
 * This lens focuses the view on the entry in a map with a given index.
 * Optional in the Monocle terminology is the same a partial lense in the
 * terminology of Foster et al.
 * 
 * Use lenses compositin to update itu the same way (move Alex to zipcode
 * 9100) but in a clearer way. Use the infix binary operator composeOptional
 * to compose a lense with an optional, and use andThen to compose
 * the optional with a lense).
 * 
 * (1-2 lines)
 */

lazy val itu2: University =
  itu.focus()                       // focus lets us start the composition of lenses
    .andThen(_students)             // then we compose the function to get and set the students map
    .andThen(index("Alex"))         // then we compose the Lens that gets the student alex and can set the student alex
    .andThen(_zipcode)              // then we compose with the lens to change a students zipcode
    .modify(_ => "9100")            // and then we change it


/* Now once you provide lenses for your types, navigating and modifying deep
 * structures becomes more readable and easier to write.  In fact, lense
 * libraries provide various mechanisms to generate them for the properties of
 * your case classess, so this access can come at almost no (coding) cost.
 */



/* Exercise 8
 *
 * Monocle provides compiler macros (a part of Scala we side step in ADPRO),
 * which allow very concise generation of lenses for case classes. For
 * instance _zipcode from above can be easily generated as follows:
 */

val _zipcode1 : Lens[Address, ZipCode] =
  Focus[Address](_.zipcode)

// Define _students1 analogously, complete itu3 to use these new lenses (with
// the same specification as itu2).

lazy val _students1: Lens[University, Map[String, Address]] =
  Focus[University](_.students)

lazy val itu3: University =
  itu.focus()
    .andThen(_students1)
    .andThen(index("Alex"))
    .andThen(_zipcode1)
    .modify(_ => "9100")

/* As a curiosity, here is an example that this can be moved even closer to
 * imperative style, using extension methods and macros:
 */

lazy val itu4: University =
  itu
    .focus(_.students)
    .at("Alex")
    .some
    .andThen(_zipcode) // focus syntax is not implemented for Optionals in monocle
    .replace("9100")

/* Note how similarly it would look in Java, imperative style
 *
 * try {
 *    itu
 *      .getStudents ()
 *      .get ("Alex")
 *      .setZipcode ("9100")
 * } catch NoSuchElementException { }
 */



/* Exercise 9
 *
 * Turn names of all countries in all the addresses of all students in the itu
 * object into uppercase. Using lenses of course.
 *
 * We shall use the 'modify' function of lenses. Morris describes the modify
 * problem in Section 2, and shows the lens solution in Listing 9.  Monocle
 * has a modify method in Lens[A, B]:
 *
 *    modify : (B => B) => A => A
 *
 * It works like a simulatnous combination of get and set at the same time.
 * We use modify if we need to get a value, and then make a modification to
 * it.  Modify takes a function that makes the change (computes the new data)
 * and then the source(concrete) object.  It returns the new object. It is
 * potentially more efficient than using get and set separately.  So modify is
 * "map" that applies at some nested level in a complex structure.
 *
 * In this exercise, we use modify to perform a cross cutting modification
 * on a university object.
 *
 * We will need a lense that gives us all countries from the map of students.
 * This kind of lense is called a Traversable in Monocle.
 *
 * We use 'andThen' to compose an optical (Lens, Traversable, Optional, etc)
 * with a traversable (as we used 'andThen' above to compose any of these with a
 * Lens).
 *
 * The traversable "each" (which has a default instance for maps) will give us a
 * collection of all objects (values in a map).  So to solve the task we need to
 * compose:
 *
 * - A lense that extracts the students collection from a University (_students)
 *
 * - A traversable that extracts all objects from a collection (each)
 *
 * - A lense that extract the country from an address object (_country, you will
 *   need to write that one, as we did not create it yet). Let's start with
 *   _country (1 line):
 */

lazy val _country: Lens[Address,String] = 
  Focus[Address](_.country)

/* Now compute itu5, that has the same entries as ITU, but all country names
 * are upper case. (1-6 lines)
 */

lazy val itu5: University =
  _students1.each.andThen(_country).modify(s => s.toUpperCase())(itu)

/* Exercise 10
 *
 * Use filterIndex(p) to only capitalize city names of the students on the
 * list whose name satisfies predicate 'p'. Let's capitalize the names of
 * students whose name begins with letter A.  The filterIndex combinator is a
 * traversal, like 'each' above. Recall that 'composeTraversal' is used to
 * compose (append) a traversal and 'andThen is used to append a lense.
 */

lazy val itu6: University =
  _students1.filterIndex((p: String) => p.charAt(0).toUpper == 'A')
    .andThen(_country)
    .modify(s => s.toUpperCase())(itu)
 
/* Exercise 11
 *
 * We are returning to the construction of basic lenses.  Implement a
 * (partial) lens that accesses the i-th element of a list (let's call it
 * 'ith').  A partial lens, so an Optional in Monocle terminology, would be
 * of type Optional[List[A],A].  The Optional takes two parameters for the
 * constructor:
 *
 * get: List[A] => Option[A], set: A => List[A] => List[A]
 *
 * You will need to decide what to do with the setter if n is greater than the
 * length of the list.  One option is to do nothing, just ignore the setting.
 * Another alternative is to provide a default element, and extend the list
 * approprietly if accessed or set beyond the size. In the latter case we
 * obtain a total lense.  We first try the partial version (ca. 5-8 lines):
 */

def ith[A](n: Int): Optional[List[A], A] =
  Optional[List[A], A]
    (xs => if xs.length > n then Some(xs(n)) else None) 
    (a => xs => if xs.length > n then xs.updated(n, a) else xs)
  
/* Now create the total lense with a default value that extends the
 * list, if the list is too short (8-15 lines):
 */

def ith1[A](default: A)(n: Int): Lens[List[A], A] =
  Lens[List[A], A] 
    (get = xs => if xs.length > n then xs(n) else default)
    (replace = a => xs => if xs.length > n then xs.updated(n, a) else 
      xs.appendedAll(List.fill((n+1) - xs.length)(default)).updated(n, a)
      )



/* Exercise 12
 *
 * The lens 'ith' demonstrates that lenses emulate a form of "imperative"
 * programming, by making a structure updatedable, even deeply.  Use the lens
 * (an Optional in Monocle) 'ith' to increment the third element on a list
 * 'list0'
 */

val list0: List[Int] = List(1, 2, 3, 4, 5, 6)

// list1 should contain list0 with the third element incremented:

lazy val list1: List[Int] = list0.focus()
                                  .andThen(ith(2))
                                  .modify(_+1)

