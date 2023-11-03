// Andrzej Wasowski, IT University of Copenhagen
// based on fpinscala exercises
package adpro

import language.higherKinds
import language.implicitConversions

import scala.util.matching.Regex

object Parsing {

  trait Parsers[ParseError,Parser[+_]] { self =>

    def flatMap[A,B] (p: Parser[A]) (f: A => Parser[B]): Parser[B]

    def or[A] (s1: Parser[A], s2: => Parser[A]): Parser[A]

    def map[A,B] (p: Parser[A]) (f: A => B): Parser[B] =
      p flatMap { a => succeed (f (a)) }

    def map2[A,B,C] (p1: Parser[A], p2: => Parser[B]) (f: (A,B) => C) =
      p1 flatMap { a => p2 map { b => f (a,b) } }

    // Will cause stack overflow if p is not consuming any chars
    def many[A] (p: Parser[A]): Parser[List[A]] =
      map2 (p, many(p)) (_::_) or succeed[List[A]] (Nil)

    def manyA: Parser[Int] =
      many ('a') map { _.size }

    def product[A,B] (p1: Parser[A], p2: => Parser[B]): Parser[(A,B)] =
      map2 (p1, p2) ((a,b) => (a,b))

    def many1[A] (p: Parser[A]): Parser[List[A]] =
      (p ** many (p)) map[List[A]] { case (h,t) => h::t }

    def listOfN[A] (n: Int, p: Parser[A]): Parser[List[A]] =
      if (n <= 0)
        succeed (List[A] ())
      else
        map2 (p, listOfN[A] (n-1, p)) (_::_)

    def slice[A] (p: Parser[A]): Parser[String]
    def succeed[A] (a: A): Parser[A]
    def fail (err: ParseError): Parser[Nothing]

    def run[A] (p: Parser[A]) (input: String): Either[ParseError,A]

    implicit def char (c: Char): Parser[Char] =
      string (c.toString) map { _ => c }

    implicit def operators[A] (p: Parser[A]): ParserOps[A] =
      ParserOps[A] (p)

    implicit def regex (r: Regex): Parser[String]

    implicit def string (s: String): Parser[String]

    implicit def asStringParser[A] (a: A)
      (implicit f: A => Parser[String]): ParserOps[String] =
        ParserOps (f (a))

    case class ParserOps[A] (p: Parser[A]) {

      def |[B>:A] (p2: => Parser[B]): Parser[B] =
        self.or (p,p2)

      def |*[B] (p2: => Parser[B]): Parser[B] =
        slice.flatMap { s => p2 }

      def *| (p2: => Parser[Any]): Parser[A] =
        (p ** p2) map { case (a,_) => a }

      def ? : Parser[Option[A]] =
        ( p map { a => Some (a) } ) | succeed (None)

      def * : Parser[List[A]] =
        self.many (p)

      def or[B>:A] (p2: => Parser[B]): Parser[B] =
        self.or (p,p2)

      def **[B] (p2: => Parser[B]): Parser[(A,B)] =
        self.product (p,p2)

      def product[B] (p2: => Parser[B]): Parser[(A,B)] =
        self.product (p, p2)

      def map[B] (f: A => B): Parser[B] =
        self.map (p) (f)

      def flatMap[B] (f: A => Parser[B]): Parser[B] =
        self.flatMap (p) (f)

      def many: Parser[List[A]] =
        self.many[A] (p)

      def slice: Parser[String] =
        self.slice (p)
    }

  } // Parsers


  case class ParseError (stack: List[(Location, String)])

  case class Location (input: String, offset: Int = 0) {

    def cursor: String = input.substring (offset)

    def advanceBy (numChars: Int): Location =
       Location (input, offset + numChars)

    lazy val line = input.slice (0, offset + 1).count (_ == '\n') + 1
    lazy val col = input.slice (0, offset + 1).lastIndexOf ('\n') match {
      case -1 => offset + 1
      case lineStart => offset - lineStart
    }

    def toError (msg: String): ParseError = ParseError (List ((this, msg)))

  }

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {

    def advanceCharsConsumed (n: Int): Result[A] =
      this match {
        case Success (get, charsConsumed) =>
          Success (get, n + charsConsumed)
        case Failure (_) => this
      }

  }

  case class Success[+A] (get: A, charsConsumed: Int) extends Result[A]
  case class Failure (get: ParseError) extends Result[Nothing]

  object MyParsers extends Parsers[ParseError, Parser] {

    def run[A] (p: Parser[A]) (input: String): Either[ParseError,A] =
      p (Location (input, 0)) match {

        case Success (a, n) => Right (a)
        case Failure (err) => Left (err)
      }

    def slice[A] (p: Parser[A]): Parser[String] =
      l => p (l) match {
        case Success (_, n) => Success (l.cursor.substring (0,n), n)
        case f @ Failure (_) => f
      }

    def fail (err: ParseError): Parser[Nothing] =
      loc => Failure (err)

    def fail: Parser[Nothing] = fail (err = ParseError (Nil))

    def flatMap[A,B] (p: Parser[A]) (f: A => Parser[B]): Parser[B] =
      loc => p (loc) match {
        case Success (a, n) =>
          f (a) (loc advanceBy n) advanceCharsConsumed (n)
        case failure @Failure (_) => failure
      }

    def succeed[A] (a: A): Parser[A] =
      loc => Success (a, 0)

    implicit def string (s: String): Parser[String] = {
      loc =>
        val c = loc.cursor
        assert (s.size == s.length)
        if (c startsWith s)
          Success (s, s.size)
        else {
          val n = Math.min (c.size, Math.max (s.size, 5))
          val m = s"Expected '$s' but seen '${c.substring (0,n)}'"
          Failure (ParseError (List (loc -> m)))
        }
    }

    def or[A] (p1: Parser[A], p2: => Parser[A]): Parser[A] =
      loc => p1 (loc) match {
        case success @ Success (_, _) => success
        case Failure (err) => p2 (loc)
      }

    implicit def regex (r: Regex): Parser[String] = {
      loc =>
        val c = loc.cursor
        (r findPrefixOf c) match {

          case Some (prefix) =>
            Success (prefix, prefix.size)

          case None =>
            val err = loc.toError (s"Regex '${r}' not matched!")
            Failure (err)
        }
    }

  }

  trait JSON

  case object JNull extends JSON
  case class JNumber (get: Double) extends JSON
  case class JString (get: String) extends JSON
  case class JBool (get: Boolean) extends JSON
  case class JArray (get: IndexedSeq[JSON]) extends JSON
  case class JObject (get: Map[String, JSON]) extends JSON

  class JSONParser[ParseError, Parser[+_]] (P: Parsers[ParseError,Parser]) {

    import P._

    lazy val QUOTED: Parser[String] =
      """"[^"]*"""".r
        .map { _.dropRight (1).substring (1) }

    lazy val DOUBLE: Parser[Double] =
      """(\+|-)?[0-9]+(\.[0-9]+((e|E)(-|\+)?[0-9]+)?)?""".r
        .map { _.toDouble }

    lazy val ws: Parser[Unit] =
      """\s+""".r.map { _ => () }

    lazy val jnull: Parser[JSON] =
      "null" |* ws.? |* succeed (JNull)

    lazy val jbool: Parser[JBool] = {
      val t = ("true"  |* ws.? |* succeed (JBool(true)))
      val f = ("false" |* ws.? |* succeed (JBool(false)))
      t | f
    }

    lazy val jstring: Parser[JString] =
      { QUOTED *| ws.? }
        .map { JString (_) }

    lazy val jnumber: Parser[JNumber] =
      { DOUBLE *| ws.? }
        .map { JNumber (_) }

    private lazy val commaSeparatedVals: Parser[List[JSON]] =
      { json ** ( "," |* ws.? |* ws.? |* json ).* }
        .map { case (h,t) => h::t }

    lazy val jarray: Parser[JArray] =
      { "[" |* ws.? |* commaSeparatedVals.? *| "]" *| ws.? }
        .map[JArray] { l => JArray (l.getOrElse (Nil).toVector) }

    lazy val field: Parser[(String,JSON)] =
      { QUOTED *| ws.? *| ":" *| ws.? ** json }

    private lazy val commaSeparatedFields: Parser[List[(String,JSON)]] =
      { field ** ( "," |* ws.? |* field ).* }
        .map { case (h,t) => h::t }

    lazy val jobject: Parser[JObject] =
      { "{" |* ws.? |* commaSeparatedFields.? *| "}" *| ws.? }
        .map { l => JObject (l.getOrElse (Nil).toMap) }

    lazy val json: Parser[JSON] =
      ws.? |* { jstring | jnumber | jnull |  jbool | jarray | jobject }

  }

}
