// Advanced Programming, Andrzej Wasowski
// Probabilistic Programming (AKA Probability is also a monad)

// run th script with `scala-cli Basic.sc`
import pigaro.*
import pigaro.given

// Used by samplers below
given rng: spire.random.rng.SecureJava 
  = spire.random.rng.SecureJava.apply

val die: Dist[Int] = Uniform("die")(1, 2, 3, 4, 5, 6)
val sample: IData[Int] = die.sample(5)

println("A small sample for " + sample)

// Slide 6

// This is a Functor, so we can map! 
// Let's what is the probability that n is prime
val prime: Dist[Boolean] =
  die.map { n => n == 2 || n == 3 || n == 5 }

// We can use for notation as well
val odd: Dist[Boolean] =
  for n <- die
  yield n % 2 == 1

val N = 10000
 
val probPrime = prime.sample(N).pr(true)
println(s"Pr. of getting a prime outcome: $probPrime")
 
val probOdd = odd.sample(N).pr(true)
println (s"Pr. of getting an odd outcome: $probOdd")

val both: IData[(Boolean, Boolean)] = (prime -> odd).sample(N)
val probBoth = both.pr[(Boolean, Boolean)] { (p, o) => p && o }
println (s"Pr. of a prime and odd outcome is $probBoth (independent)")

val primeAndOdd: IData[Int] = die.sample(N)
val probPrimeAndOdd = primeAndOdd.pr { o =>  o == 3 || o == 5 }
println (s"Pr. of a prime and odd outcome: $probPrimeAndOdd (dependent)")

// The same via hierarchical composition 
// (we have to put all the elements in the same model)
// _map takes a new variable name optionally
val primeAndOddViaComposition = 
  Pigaro
   .uniform("die")         (1, 2, 3, 4, 5, 6)
   .detDep ("prime")       { n => n == 2 || n == 3 | n == 5 }
   .detDep ("odd")         { (n, prime) => n % 2 == 1 }
   .detDep ("prime & odd") { (n, prime, odd) => prime && odd }

println(primeAndOddViaComposition.sample(4))

val probPrimeAndOddViaComposition = 
  primeAndOddViaComposition
   .sample(N)
   .pr(true)

println(s"Probability of a prime and odd outcome: $probPrimeAndOddViaComposition (dependent, hierarchical)")


// Now we can divide 0.33.../ 0.5...getting approximate 2/3
println(s"Pr. of a prime given an odd: ${probPrimeAndOdd / probOdd} (manual)")


// The same obtained via conditioning
val primeGivenOdd = Pigaro
  .uniform("die")       (1, 2, 3, 4, 5, 6)
  .detDep ("prime")     { n => n == 2 || n == 3 | n == 5 }
  .detDep ("odd")       { (d, prime) => d % 2 == 1 }
  .condition            { _._3 }
  .map    ("prime|odd") { (n, prime, odd) => prime }

val probPrimeGivenOdd = 
  primeGivenOdd.sample(N).pr(true) 
println(s"Pr. of a prime given an odd: $probPrimeGivenOdd (auto)")

enum Child:
  case Boy, Girl
  def fromInt (n: Int): Child = 
    if n % 2 == 0 then Girl else Boy
  def toBool: Boolean = this match
    case Boy => true
    case Girl => false


// Boy-Girls example (from Jes Frellsen, BSWU)

import Child.*
 
val child = Uniform[Int](0,1).map(Child.fromOrdinal)
val S = child -> child

val E: Dist[Boolean] = 
  S.map { case (Boy,Boy) => true; case _ => false }
val F: Dist[Boolean] = 
  S.map { case (f,s) => f == Boy || s == Boy }

val prE = E.sample(N).pr(true)
val prF = F.sample(N).pr(true)

println(s"Pr. of getting two boys: $prE")
println(s"Pr. of getting at least on boy: $prF")
println(s"Pr. of two Boys given at least one boy: ${prE/prF}")
// The above cheats slightly (exploits the fact that E * F = E)
// It also is very manual. We could use the framework to calculate
// this instead


val model1 = Pigaro
  .uniform ("fst") (Boy, Girl)
  .uniform ("snd") (Boy, Girl)
  .condition       { case (fs,sn) => fs == Boy || sn == Boy }

val prBBGivenBoy = 
  model1.sample(N).prMatching { case (Boy,Boy) => }

println(s"Pr. of 2 Boys given at least 1 via condition: $prBBGivenBoy")

// Example with Balls

enum Box: 
  case A, B 

enum Ball: 
  case Red, Green

import Ball.*

val model2 = for
  box  <- Bernoulli[Box]("box", .5, Box.A, Box.B)
  p     = if box == Box.A then 2.0/9.0 else 4.0/7.0
  ball <- Bernoulli[Ball]("ball", p, Green, Red)
yield (box, ball)

val prBoxA = model2
  .matching { case (_, Red) => }
  ._1
  .sample(N)
  .pr(Box.A)

println(s"Pr. of Box A given a Red ball is $prBoxA (for-yield)")

// Roughly the same without for-yield (AW finds this much easier, 
// especially that conditioning can be done in the same block)
// Note that we are using a new convenient conditioning function
// `matching`

val model3 = Pigaro
  .bernoulli("box")  (.5, Box.A, Box.B)
  .detDep   ("p")    { box => if box == Box.A then 2.0/9.0 else 4.0/7.0 }
  .bernoulli("ball") (Green, Red) { (b,p) => p }
  .matching          { case (_, _, Red) => }
  ._1

val prBoxA3 = model3.sample(N).pr(Box.A)
println(s"Pr. of Box A given a Red ball is $prBoxA (dsl)")

// Expectation queries

val model4  = Uniform(1,2,3,4,5,6)
val sample4 = model4.sample(N)
val mean    = sample4.mean
val median  = sample4.median

println(s"Expected outcome of an unbiased die: $mean")
println(s"Sample median outcome of an unbiased die: $median")
