// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro

import scala.quoted.*
import scala.quoted.FromExpr.*

inline def annotations[T] (fname: String): Seq[String] =
  ${annotationsImpl[T]('fname)}

def annotationsImpl[T: Type] (fname: Expr[String]) (using Quotes)
  : Expr[Seq[String]] =
  import quotes.reflect.*
  val result = for
    f <- TypeRepr.of[T]
          .typeSymbol
          .methodMember(fname.value.get)
          .headOption
    as = f.annotations.map { _.tpe.show }
  yield as
  Expr { result getOrElse Seq() }
