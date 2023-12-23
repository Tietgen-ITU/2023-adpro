// Advanced Programming, Exercises by A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro.parsing

import scala.util.matching.Regex

import org.scalacheck.*
import org.scalacheck.Prop.*

trait Parsers[ParseError, Parser[+_]]:
  self =>

  def string(s: String): Parser[String]
  def char(c: Char): Parser[Char] =
    string(c.toString).map { _.charAt(0) }

  /** A default `succeed` implementation in terms of `string` and `map`. We
    * leave `succeed` abstract, since `map` is defined below in terms of
    * `flatMap` and `succeed`, which would be a circular definition! But we
    * include the definition here in case implementations wish to use it (say
    * if they provide a custom implementation of `map`, breaking the cycle)
    */
  def defaultSucceed[A](a: A): Parser[A] =
    string("").map { _ => a }

  def succeed[A](a: A): Parser[A]
  def empty: Parser[Unit] 

  def regex(r: Regex): Parser[String]

  extension [A](p: Parser[A])
    def run(input: String): Either[ParseError, A]
    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or (p2)
    def slice: Parser[String] 
    def flatMap[B](f: A => Parser[B]): Parser[B]
  end extension


  extension [A](p1: Parser[A]) 
    def map2[B, C](p2: => Parser[B]) (f: (A, B) => C): Parser[C] =
      p1.flatMap { a => p2.flatMap { b => succeed(f(a, b)) } }

    def product[B] (p2: => Parser[B]): Parser[(A,B)] =
      p1.map2(p2) { (a, b) => (a, b) }

    def **[B](p2: => Parser[B]): Parser[(A, B)] = 
      p1.product(p2)
    def |*[B](p2: => Parser[B]): Parser[B] =
      p1.flatMap { s => p2 }
    def *| (p2: => Parser[Any]): Parser[A] =
      (p1 ** p2).map { (a, _) => a }
    def ? : Parser[Option[A]] =
      { p1.map { Some(_) } } | succeed(None)
    def * : Parser[List[A]] = 
      p1.many
  end extension

  extension [A](p: Parser[A]) 
    def many: Parser[List[A]] = 
      p.map2(p.many) { _ :: _ } | succeed(Nil)

  extension [A](p: Parser[A])
    def map[B](f: A => B): Parser[B] =
      p.flatMap { a => succeed(f(a)) }

  // A better name would be: howManyA
  def manyA: Parser[Int] =
    many(char('a')).map { _.size }

  extension [A](p: Parser[A]) 
    def many1: Parser[List[A]] =
       p.map2(many(p)) { _ :: _ }
      

  extension [A](p: Parser[A]) 
    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(List[A]())
      else p.map2(p.listOfN(n - 1)) { _::_ }

end Parsers

// A concrete implementation for our parsers, with optimized slicing and
// committing (the late variant in the chapter).  It provides all the 
// basic operator implementations for Parsers and detailed representation
// of the parser data structures.

case class ParseError(stack: List[(Location, String)] = Nil):

  def push(loc: Location, msg: String): ParseError =
    this.copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map { (_, s) }.toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest.map { _._1 }

  /** Display collapsed error stack - any adjacent stack elements with the
    * same location are combined on one line. For the bottommost error, we
    * display the full line, with a caret pointing to the column of the error.
    * Example:
    * 1.1 file 'companies.json'; array
    * 5.1 object
    * 5.2 key-value
    * 5.10 ':'
    * { "MSFT" ; 24,
    *          ^
    */
  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
        collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"

end ParseError

case class Location(input: String, offset: Int = 0):

  /** The line number in the current input at the position offset */
  lazy val line: Int = 
    input.slice(0, offset + 1).count { _ == '\n' } + 1

  /** The column number in the current input at the position offset */
  lazy val col: Int = 
    input.slice(0, offset + 1).lastIndexOf('\n') match
      case -1 => offset + 1
      case lineStart => offset - lineStart

  /** Promote a location and a message to a ParseError */
  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  /** Shift this location by n characters forward */
  def advanceBy(n: Int) = copy(offset = offset + n)

  /** The remaining input starting at this location */
  def remaining: String = input.substring(offset)

  /** A slice of the input starting at this location and having n chars */
  def slice(n: Int): String = input.substring(offset, offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next() else ""
    else ""

  def columnCaret = (" " * (col - 1)) + "^"

end Location

/** `isSliced` indicates if the current parser is surround by a `slice`
  * combinator. This lets us avoid building up values that will end up getting
  * thrown away. Mostly convenience functions used below. */
case class ParseState(loc: Location, isSliced: Boolean = false):
  def advanceBy(numChars: Int): ParseState =
    copy(loc = loc.advanceBy(numChars))
  def input: String = loc.input.substring(loc.offset)
  def unslice = copy(isSliced = false)
  def reslice(s: ParseState) = copy(isSliced = s.isSliced)
  def slice(n: Int) = loc.input.substring(loc.offset, loc.offset + n)
end ParseState

type Parser[+A] = ParseState => Result[A]

/** The result of a parse--a `Parser[A]` returns a `Result[A]`.
  *
  * There are three cases:
  *   - Success(a, n): a is the value, n is # of consumed characters
  *   - Slice(n): a successful slice; n is the # of consumed characters
  *   - Failure(n, isCommitted): a failing parse
  *
  * As usual, we define some helper functions on `Result`.
  * 
  * `Result` is an example of a Generalized Algebraic Data Type (GADT),
  * which means that not all the data constructors of `Result` have
  * the same type. In particular, `Slice` _refines_ the `A` type
  * parameter to be `String`. If we pattern match on a `Result`
  * and obtain a `Slice`, we expect to be able to assume that `A` was
  * in fact `String` and use this type information elsewhere.
  */
enum Result[+A]:
  case Success(get: A, length: Int)
  case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]
  case Slice(length: Int) extends Result[String]

  /** A helper to extract the value from the result */
  def extract(input: String): Either[ParseError, A] = this match
    case Slice(length) => Right(input.substring(0, length))
    case Success(get, _) => Right(get)
    case Failure(get, _) => Left(get)

  /** Convert a parse result to a slice (failures remain failures). */
  def slice: Result[String] = this match
    case s @ Slice(_) => s
    case Success(_, length) => Slice(length)
    case f @ Failure(_, _) => f

  /* Used by `attempt`. */
  def uncommit: Result[A] = this match
    case Failure(e, true) => Failure(e, false)
    case _ => this

  /* Used by `flatMap` */
  def addCommit(isCommitted: Boolean): Result[A] = this match
    case Failure(e, c) => Failure(e, c || isCommitted)
    case _ => this

  /* Used by `scope`, `label`. */
  def mapError(f: ParseError => ParseError): Result[A] = this match
    case Failure(e, c) => Failure(f(e), c)
    case _ => this

  /** Adjust the number of characters consumed while producing the result. `n`
    * is the number of chars consumed by this particular parser that produced
    * the result (the total number of consumed. could be higher if other
    * parsers have already consumed some part of the input). */
  def advanceSuccess(n: Int): Result[A] = this match
    case Slice(length) => Slice(length + n)
    case Success(get, length) => Success(get, length + n)
    case Failure(_, _) => this

end Result

object Sliceable 
  extends Parsers[ParseError, Parser]:
  
  import Result.{Slice, Success, Failure}

  /** Consume no characters and succeed with the given value */
  def succeed[A](a: A): Parser[A] =
    (s: ParseState) => Success(a, 0)

  def string(w: String): Parser[String] = (s: ParseState) =>
    val i = firstNonmatchingIndex(s.loc.input, w, s.loc.offset)
    if i == -1 then // they matched
      if s.isSliced then Slice(w.length)
      else Success(w, w.length)
    else Failure(s.loc.advanceBy(i).toError(s"'$w'"), i != 0)

  def empty: Parser[Unit] = (s: ParseState) => 
    if s.loc.remaining.isEmpty 
    then Success((), 0) 
    else Failure(s.loc.toError(s"'end-of-input"), false)

  /** Returns -1 if s.startsWith(s2), otherwise returns the
    * first index where the two strings differed. If s2 is
    * longer than s, returns s.length. Note: locally imperative. */
  def firstNonmatchingIndex(s: String, s2: String, offset: Int): Int =
    var i = 0
    while (i + offset < s.length && i < s2.length)
      if s.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s.length - offset >= s2.length then -1
    else s.length - offset

  def fail(msg: String): Parser[Nothing] =
    (s: ParseState) => Failure(s.loc.toError(msg), true)


  extension [A](p: Parser[A]) 

    def run(s: String): Either[ParseError, A] =
      p(ParseState(Location(s), false)).extract(s)

    /* This implementation is rather delicate. Since we need an `A`
     * to generate the second parser, we need to run the first parser
     * 'unsliced', even if the `flatMap` is wrapped in a `slice` call.
     * Once we have the `A` and have generated the second parser to
     * run, we can 'reslice' the second parser.
     *
     * Note that this implementation is less efficient than it could
     * be in the case where the choice of the second parser does not
     * depend on the first (as in `map2`). In that case, we could
     * continue to run the first parser sliced.
     */
    def flatMap[B](f: A => Parser[B]): Parser[B] = (s: ParseState) => 
      p(s.unslice) match
      case Success(a, n) =>
        f(a)(s.advanceBy(n).reslice(s))
          .addCommit(n != 0)
          .advanceSuccess(n)
      case Slice(n) =>
        f(s.slice(n))(s.advanceBy(n).reslice(s)).advanceSuccess(n)
      case f @ Failure(_, _) => f

    def slice: Parser[String] = 
      (s: ParseState) => p(s.copy(isSliced = true)).slice

  end extension

 
  extension [A](p: Parser[A]) 
    def or(p2: => Parser[A]): Parser[A] =
      (s: ParseState) => p(s) match
      case Failure(e, false) => p2(s)
      case r => r // committed failure or success skips running `p2`


  def regex(r: Regex): Parser[String] = (s: ParseState) =>
    r.findPrefixOf(s.loc.remaining) match
    case Some(consumed) =>
      if s.isSliced then Slice(consumed.length)
      else Success(consumed, consumed.length)
    case None => 
      Failure(s.loc.toError(s"regex $r"), false)
   
end Sliceable
