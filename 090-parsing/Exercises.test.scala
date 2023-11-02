// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// Based on Functional Programming in Scala, 2nd Edition

package adpro.parsing

import org.scalacheck.*
import org.scalacheck.Prop.{forAll, propBoolean, forAllNoShrink}
import org.scalacheck.Arbitrary.arbitrary

import Sliceable.*
import Result.{Success, Slice, Failure}

given Arbitrary[Location] =
  Arbitrary { for
    s <- summon[Arbitrary[String]].arbitrary
    n <- Gen.choose (0, s.size)
  yield Location (s,n) }

given Arbitrary[ParseState] =
  Arbitrary { for l <- summon[Arbitrary[Location]].arbitrary 
      c <- Gen.oneOf(true, false)
  yield ParseState(l,c) } 

// Some concrete test-cases

val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

object ParsingSpec
  extends org.scalacheck.Properties("parsing"):
  
  // Exercise 1 (map2, product via flatMap)

  property("Ex01.01: map2 with two successes") =
    Laws.map2withTwoSuccesses

  // Slightly annoying with the current design that failure tests cannot
  // be written abstractly because we have not abstracted failures,
  // and have no way to create a default error message.  This is, of
  // course, possible to do, but I prefer not to depart from the book
  // so far.

  property("Ex01.02: map2 with right failure") =
    val p = succeed(42).map2[Int, Int](fail("failure")) { _+_ }
    forAll { (input: String) => p.run(input).isLeft }

  property("Ex01.03: map2 with left failure") =
    val p = (fail("failure"): Parser[Int])
      .map2[Int,Int](succeed (42)) { _+_ }
    forAll { (input: String) => p.run(input).isLeft }

  property("Ex01.04: map2 regression 'aaax'") =
    val p = string("a")
    val p2 = p.map2(p) { (a, b) => List (a, b) }
    val p3 = p.map2(p2) { _:: _ }
    p3.run("aaax") == Right(List("a", "a", "a"))

  property("Ex01.05: map2 regression 'aaax' with succeed") =
    val p = string("a")
    val p2 = p.map2(p) { (a, b) => List(a, b) }
    val p3 = succeed("a").map2(p2) { _::_ }
    p3.run("aaax") == Right(List("a", "a", "a"))

  property("Ex01.06: map2 regression 'aax'") =
    val p = char('a')
    val p2 = p.map2(p) { (a, b) => List (a, b) }
    p2.run("aaax") == Right(List('a', 'a'))

  // This gets also tested with listOfN
  property("Ex01.07: product success") = 
    val ps = ParseState(Location ("xaabb", 1))
    (string("aa") ** string ("bb")) (ps) == Success("aa" -> "bb", 4)

  property("Ex01.08: product right fail") =
    val ps = ParseState(Location ("xaacc", 1))
    (string("aa") ** string("bb")) (ps) match
    case Failure(_, _) => true
    case _ => false

  property("Ex01.09: product left fail") =
    val ps = ParseState(Location ("xccbb", 1))
    (string("aa") ** string("bb")) (ps) match 
    case Failure(_, _) => true
    case _ => false

  // Exercise 2 (many)

  property("Ex02.01: many can multiply any string parser") =
    Laws.manyString

  property("Ex02.02: many regression 'aaax'") =
    many(char('a')).run ("aaax") == Right(List('a', 'a', 'a'))

  // Exercise 3 (map)

  property("Ex03.01: Simplified structure preservation") = 
    Laws.mapStructurePreserving2succeed

  property("Ex03.02: Simplified structure preservation") = 
    Laws.map2withTwoSuccesses

  // This is really a test for flatMap, but since we do not really
  // have an exercise on flatMap (it is pre-solved), it can stay here.
  property("Ex03.03: map keeps the # of consumed chars (regression)") = 
    forAll { (l: ParseState) =>
      val k = Math.min(5, l.loc.remaining.size)
      val s = l.loc.remaining.substring(0, k)
      val p = string(s)
      val pm = p.map { _ => 42 }

      p(l) -> pm(l) match
      case (Success(t, n), Success (42,m)) =>
        Prop(n == m) && Prop(t == s)
      case (Failure (_,_), Failure (_,_)) =>
        Prop(false)
      case (Slice(_), Slice(_)) =>
        Prop(false) :| "This case should not be happening"
      case (Success(_, _), Failure(_, _)) =>
        Prop(false) :| "Success vs Failure"
      case (Failure(_, _), Success(_, _)) =>
        Prop(false) :| "Failure vs Success"
      case (Success(_, _), Slice(_)) =>
        Prop(false) :| "Success vs Slice"
      case (Slice(n), Success(42, m)) =>
        Prop(l.isSliced) :| "only happens if starting in sliced state"
          && Prop(n == m) :| "both parsers use the same # of chars"
      case _ => 
        Prop(false) :| "This case happening means a bug"
    }

  property("Ex03.04: map and failure") =
    val err = ParseError(Nil)
    val f = fail("error message")
    val p: Parser[Int] = f.map[Int] { _ => 42 }
    forAll { (l: ParseState) => 
      val r1: Result[Int] = p(l)
      val r2: Result[Int] = f(l)
      { r1 == r2 } :| s"obtained $r1 expected: $r2" }

  // Exercise 4 (manyA)

  property("Ex04.01: manyA positive test") =
    forAll (Gen.choose(0, 1000)) { n =>
      val s = "a" * n + "x"
      manyA.run(s) == Right(n)
    }

  property("Ex04.02: manyA regression 'aaax'") =
    manyA.run("aaax") == Right(3)

  property("Ex04.03: manyA regression 'aax'") =
    manyA.run("aax") == Right(2)

  property("Ex04.04: manyA regression 'ax'") =
    manyA.run("ax") == Right(1)

  property("Ex04.05: manyA regression 'x'") =
    manyA.run("x") == Right(0)

  // Exercise 5 (many1)

  property("Ex05.01: many1 success") =
    Laws.many1Success

  property("Ex05.02: many1 fail") =
    Laws.many1Fail

  property("Ex05.03: string 'ax' regression (not many1)") =
    string("a").run("ax") == Right("a")

  property("Ex05.04: many1 'xa' regression") =
    string("a").many1.run("xa").isLeft
 
  property("Ex05.05: many1 'aax' regression") =
    string("a").many1.run("axa") == Right(List("a"))
 
  property("Ex05.06: many1 'aaxa' regression") =
    string("a").many1.run("aaxa") == Right(List("a", "a"))
 
  property("Ex05.07: many1 'aaaxa' regression") =
    string("a").many1.run("aaaxa") == Right(List("a", "a", "a"))

  // Exercise 6 (listOfN)

  property("Ex06.01: listOfN 1") = Laws.listOfN1

  property("Ex06.02: listOfN 2") =
    Laws.listOfN2

  property("Ex06.03: listOfN 3") =
    Laws.listOfN3

  property("Ex06.04: listOfN 4") =
    Laws.listOfN4

  property("Ex06.05: listOfN 5") =
    Laws.listOfN5

  property("Ex06.06: listOfN fail") =
    Laws.listOfNFail

  // Exercise 7 (digitTimesA)

  property("Ex07.01: digitTimesA positive (fails until regex done)") =
    digitTimesA.run("3aaax").isRight

  property("Ex07.02: digitTimesA negative (fails until regex done)") =
    digitTimesA.run("3aax").isLeft

  property("Ex07.03: digitTimesA negative (fails until regex done)") =
    digitTimesA.run("0x").isRight


  // Exercise 8 (succeed)
    
  property("Ex08.01: The succeed law from the book (needs run)") = 
    Laws.succeed

  property("Ex08.02: succeed observes location offset") = 
    forAll { (pState: ParseState, n: Int) =>
      succeed(n)(pState) match 
      case Result.Success(get, charsConsumed) => 
        Prop(get == n) :| "wrong value of 'Success.get'"
          && Prop(charsConsumed == 0) 
          :| "'succeed' shouldn't consume chars"
      case _ => Prop(false)
    }

  // Exercise 9 (or)"

  property("Ex09.01: or left") = Laws.orLeft

  property("Ex09.02: or right") = Laws.orRight

  property("Ex09.03: or fail") = Laws.orFail

  // Exercise 10 (regex)

  property("Ex10.01: regex positive") =
    val r = "(a|b)+".r
    given Arbitrary[Int] = Arbitrary(Gen.choose(1,100))
    forAllNoShrink { (n: Int, m: Int, sliced: Boolean) =>
      val s = "b" * n + "a" * m + "x"
      val l = ParseState(Location(s, 0), sliced)
      val result = regex(r)(l)
      val expected = s.dropRight(1)
      if sliced
      then { result == Slice(s.size - 1) } :| "sliced fails"
      else { result == Success(expected,s.size-1) } :| "not sliced fails"
    }

  property("Ex10.02: regex negative") =
    val r = """(a|x)+""".r
    forAll { (l: ParseState) =>
      r.findPrefixOf(l.input).isEmpty ==> 
        regex(r)(l).extract("").isLeft }

  // Exercise 11 (tokens QUOTED, DOUBLE, ws)

  val JP = JSONParser(Sliceable)

  property("Ex11.01: QUOTED positive") =
    val s = """"Company name""""
    val result = JP.QUOTED.run(s) 
    val expected = Right("Company name")
    { result == expected } :| s"expected $expected got $result"

  property("Ex11.02: QUOTED negative test cases") =
    JP.QUOTED.run("Unquoted Company Name").isLeft

  property("Ex11.03: DOUBLE positive test cases") = 
    forAll { (x: Double) =>
      val s = x.abs.toString.toLowerCase
      JP.DOUBLE.run(s) == Right(s.toDouble)
        && JP.DOUBLE.run("+" + s) == Right(s.toDouble)
        && JP.DOUBLE.run("-" + s) == Right(-s.toDouble)
    }

  property("Ex11.04: DOUBLE negative") =
    JP.DOUBLE.run("e12343").isLeft

  property("Ex11.05: ws positive") =
    forAll { (n1: Int, s1: String) =>
      val n = (n1 % 100).abs + 1
      val s = "\t\n " * n + s1
      val l = ParseState(Location(s, 0))
      JP.ws(l) match
      case Success((), m) => 
        Prop(m >= 3*n)
      case _ => 
        false :| "Should not happen!"
    }

  property("Ex11.06: ws negative") =
    JP.ws.run("e12343 \t\n").isLeft

  // Exercise 12 (terminals)

  import JSON.*

  property("Ex12.01: jnumber positive") =
    val s = "-0.42e+42"
    JP.jnumber.run(s) == Right(JNumber(-0.42e+42))

  property("Ex12.02: jnumber negative") =
    val s = """Company name"""
    JP.jnumber.run(s).isLeft 

  property("Ex12.03: jstring positive") =
    val s = """"Company name""""
    JP.jstring.run(s) == Right(JString("Company name"))

  property("Ex12.04: jstring negative") =
    val s = """Company name"""
    JP.jstring.run(s).isLeft

  property("Ex12.05: jbool positive") =
    JP.jbool.run("true") == Right(JBool(true))

  property("Ex12.06: jbool positive") =
    JP.jbool.run("false") == Right(JBool(false))

  property("Ex12.07: jbool negative") =
    JP.jbool.run(" null").isLeft 

  property("Ex12.08: jbool negative") =
    JP.jbool.run("42").isLeft

  property("Ex12.09: jnull positive") =
    JP.jnull.run("null") == Right(JNull)

  property("Ex12.10: jnull negative") =
    JP.jnull.run(" null").isLeft

  property("Ex12.11: jnull negative") =
    JP.jnull.run("42").isLeft
  
  // Exercise 13 (non-terminals)

  property("Ex13.01: json positive") =
    JP.json.run(jsonTxt) == Right(
      JObject(Map(
        "Shares outstanding" -> JNumber(8.38E9),
        "Price" -> JNumber(30.66),
        "Company name" -> JString("Microsoft Corporation"),
        "Related companies" -> JArray(
          Vector(JString("HPQ"), JString("IBM"),
            JString("YHOO"), JString("DELL"),
            JString("GOOG"))),
        "Ticker" -> JString("MSFT"),
        "Active" -> JBool(true))))

  property("Ex13.02: json negative") =
    JP.json.run(malformedJson1).isLeft

  property("Ex13.03: json negative") =
    JP.json.run(malformedJson2).isLeft

  property("Ex13.04: jobject positive") =
    (JP.ws |* JP.jobject).run(jsonTxt) == Right(
      JObject(Map(
        "Shares outstanding" -> JNumber(8.38E9),
        "Price" -> JNumber(30.66),
        "Company name" -> JString("Microsoft Corporation"),
        "Related companies" -> JArray(
          Vector(JString("HPQ"), JString("IBM"),
            JString("YHOO"), JString("DELL"),
            JString("GOOG"))),
        "Ticker" -> JString("MSFT"),
        "Active" -> JBool(true))) )

  property("Ex13.05: jobject negative") =
    JP.jobject.run(malformedJson1).isLeft

  property("Ex13.06: jobject negative") =
    JP.jobject.run(malformedJson2).isLeft

  property("Ex13.07: field positive") =
    val s = """"price":30.66"""
    JP.field.run(s) == Right("price" -> JNumber (30.66))

  property("Ex13.08: field negative") =
    val s = """"hpq" :"""
    JP.field.run(s).isLeft

  property("Ex13.09: jarray positive") =
    val s = """[ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]"""
    JP.jarray.run(s) == Right (
      JArray (Vector (
        JString ("HPQ"),
        JString ("IBM"),
        JString ("YHOO"),
        JString ("DELL"),
        JString ("GOOG")
      )))

  property("Ex13.10: jarray negative") =
    val s = """[ "DELL" ++ "GOOG" ]"""
    JP.jarray(ParseState(Location(s, 0))) match
    case Failure(_, _) => true
    case _ => false

  property("Ex13.11: jarray empty input") =
    JP.jarray(ParseState(Location("", 0))) match
    case Failure(_, _) => true
    case _ => false

  // No Exercise (string), kept here for maintainability

  property("stri.01: string positive (Laws.runString)") =
    Laws.runString

  property("stri.02: string empty") =
    Laws.stringEmpty

  property("stri.03: string negative") =
    Laws.stringNegative

  property("stri.04: string one") =
    val l = ParseState(Location("aaa", 0))
    string("a")(l) == Success ("a", 1)
  
  // No Exercise (char)

  property("char.01: runChar (book)") = Laws.runChar

  property("char.02: char consumes one char") =
    val p = char('a')
    val l = ParseState(Location("aaa", 0), false)
    p(l) == Success('a', 1)

  property("char.03: char consumes one char") =
    val p = char('a')
    val l = ParseState(Location("aaa", 0), true)
    p(l) == Success('a', 1)

  property("char.04: char consumes one char") =
    val p = char('a')
    val l = ParseState(Location("aaa", 0), true)
    p(l.advanceBy(1)) == Success('a', 1)

  property("char.05: char consumes one char") =
    val p = char('a')
    val l = ParseState(Location("aaa", 0), true)
    p (l.advanceBy(2)) == Success('a', 1)

  // No Exercise (*| |*)

  property("*||*.01: *|") =
    val p  = string("abra") *| string("cadabra")
    p(ParseState(Location("xabracadabra", 1))) == Success("abra", 11)

  property("*||*.02: |*") =
    val p  = string("abra") |* string("cadabra")
    p(ParseState(Location("xabracadabra", 1))) == Success("cadabra", 11)

  // No Exercise (?)

  property("?___.02: ?") =
    val p  = string("abra") |* string("cadabra").?
    p(ParseState(Location ("xabra", 1))) == Success(None, 4)

  property("?___.02: ?") =
    val p  = string("abra") |* string("cadabra").?
    p(ParseState(Location("xabracadabra", 1))) 
      == Success(Some ("cadabra"), 11)

  // No Exercise (*)

  property("*___.01: *") =
    val p = string("abra").*
    p(ParseState(Location("xcadabra",1))) == Success(Nil, 0)

  property("*___.02: *") =
    val p = string("abra").*
    p(ParseState(Location("xabra",1))) == Success(List("abra"), 4)

  property("*___.03: *") =
    val p = string("abra").*
    p(ParseState(Location("xabraabracadabra", 1))) 
      == Success(List("abra", "abra"), 8)


end ParsingSpec
