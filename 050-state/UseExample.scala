package adpro.state

import State.*
import RNG.*

// generator of random integers
val random_int = State[RNG, Int] { _.nextInt } 

val (i1,r1) = random_int.run(SimpleRNG(42))
val (i2,r2) = random_int.run(r1)
