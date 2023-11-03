package fpinscala.answers.monoids

trait Monoid[A]:
  def combine(a1: A, a2: A): A
  def empty: A

object Monoid:

  val stringMonoid: Monoid[String] = new:
    def combine(a1: String, a2: String) = a1 + a2
    val empty = ""

  def listMonoid[A]: Monoid[List[A]] = new:
    def combine(a1: List[A], a2: List[A]) = a1 ++ a2
    val empty = Nil

  val intAddition: Monoid[Int] = new:
    def combine(x: Int, y: Int) = x + y
    val empty = 0

  val intMultiplication: Monoid[Int] = new:
    def combine(x: Int, y: Int) = x * y
    val empty = 1

  val booleanOr: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x || y
    val empty = false

  val booleanAnd: Monoid[Boolean] = new:
    def combine(x: Boolean, y: Boolean) = x && y
    val empty = true

  // Notice that we have a choice in how we implement `combine`.
  // We can compose the options in either order. Both of those implementations
  // satisfy the monoid laws, but they are not equivalent.
  // This is true in general--that is, every monoid has a _dual_ where the
  // `combine` combines things in the opposite order. Monoids like `booleanOr` and
  // `intAddition` are equivalent to their duals because their `combine` is commutative
  // as well as associative.
  def optionMonoid[A]: Monoid[Option[A]] = new:
    def combine(x: Option[A], y: Option[A]) = x orElse y
    val empty = None

  // We can get the dual of any monoid just by flipping the `combine`.
  def dual[A](m: Monoid[A]): Monoid[A] = new:
    def combine(x: A, y: A): A = m.combine(y, x)
    val empty = m.empty

  // Now we can have both monoids on hand
  def firstOptionMonoid[A]: Monoid[Option[A]] = optionMonoid
  def lastOptionMonoid[A]: Monoid[Option[A]] = dual(firstOptionMonoid)

  def combineOptionMonoid[A](f: (A, A) => A): Monoid[Option[A]] = new:
    def combine(x: Option[A], y: Option[A]) = x.map2(y)(f)
    val empty = None

  extension [A](optA: Option[A]) def map2[B, C](optB: Option[B])(f: (A, B) => C): Option[C] =
    for
      a <- optA
      b <- optB
    yield f(a, b)

  // There is a choice of implementation here as well.
  // Do we implement it as `f compose g` or `f andThen g`? We have to pick one.
  def endoMonoid[A]: Monoid[A => A] = new:
    def combine(f: A => A, g: A => A) = f andThen g
    val empty = identity

  import fpinscala.answers.testing.{Prop, Gen}
  import Gen.`**`

  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    val associativity = Prop.forAll(gen ** gen ** gen) { case a ** b ** c =>
      m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
    }.tag("associativity")
    val identity = Prop.forAll(gen) { a =>
      m.combine(a, m.empty) == a && m.combine(m.empty, a) == a
    }.tag("identity")
    associativity && identity

  def combineAll[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.empty)(m.combine)

  // Notice that this function does not require the use of `map` at all.
  // All we need is `foldLeft`.
  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.empty)((b, a) => m.combine(b, f(a)))

  // The function type `(A, B) => B`, when curried, is `A => (B => B)`.
  // And `B => B` is a monoid for any `B` (via function composition).
  // We have to flip the order of params to the endo monoid since we
  // chose to define it as `f andThen g` instead of `f compose g`.
  def foldRight[A, B](as: List[A])(acc: B)(f: (A, B) => B): B =
    foldMap(as, dual(endoMonoid))(f.curried)(acc)

  // Folding to the left is the same except we flip the arguments to
  // the function `f` to put the `B` on the correct side.
  def foldLeft[A, B](as: List[A])(acc: B)(f: (B, A) => B): B =
    foldMap(as, endoMonoid)(a => b => f(b, a))(acc)

  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B =
    if as.length == 0 then
      m.empty
    else if as.length == 1 then
      f(as(0))
    else
      val (l, r) = as.splitAt(as.length / 2)
      m.combine(foldMapV(l, m)(f), foldMapV(r, m)(f))

end Monoid
