# Advanced Programming, Mini-Project in Reinforcement Learning
_Andrzej Wąsowski, IT University of Copenhagen 2027_

__Deadline__: 2023-12-07 at 11.59am (noon)

This project is relatively new so suggestions of improvement
are welcomed. I apologize for typos and mistakes upfront.

Read the entire instruction before starting to get an overview

## Outline

The task is to implement a simple tabular Q-Learning, a reinforcement
learning method. This includes:

* __Reading the background material__, and interacting with teachers for
  help (if you ask on Discord, more people will benefit from your
  questions - we will do our best to be responsive)

* __Implementing the Q-Learning algorithm__ including an __interface
  for agents__

* __Implementing some property-based tests for Q-Learning__

* __Implementing a simple agent example__

* __Testing the agent__

* __Creating a program that trains the agent and prints the resulting
  policy__

All implementations must be pure, except for the final printing of the
policy (see below).

## Background Reading

We implement Q-Learning + Cliff Walking agent.

The base reading is http://www.incompleteideas.net/book/the-book-2nd.html
(the PDF of the book is on the above website, freely available).

The basis for the project is Section 6.5 starting on p. 131, but you
may need to loosely read the earlier sections in the chapter. The
algorithm to implement is in the bottom of p. 131.

The Agent to implement is the Example 6.6 on p. 132. There is no point
to read beyond page 132.

A YouTube lecture summarizing Q-Learning:
https://www.youtube.com/watch?v=__t2XRxXGxI

(Feel free to explore other videos, but beware that we are interested
only in simple discrete Q-Learning for this exercise, not the deep
reinforcement learning.)

## Implementing Q-Learning

Here are some assorted notes to get you started

* Identify inputs the parameters (α, ε, and γ). Note they all range
  over values between 0 and 1.  For our example problem, as stated on page
  131 ('undiscounted') γ is set to one, so you can simply
  ignore it. (The pseudocode in the book seems to forget to declare
  it.)

* A Q-table can be represented in several ways. The easiest is
  probably `Map[S, Map[A, Double]]` where S is the type of states of
  the agent, and A is a type of actions of the agent (this is what I
  used, where the Map is immutable).

* Your implementation should be generic in A and S (so that different
  problems can be implemented).

* The initial Q-Table can be just initialized to contain zeros on all
  positions.

* A Q-Learning algorithm does `N` episodes, each episode has a sequence
  of epochs starting with an initial state of the problem, advancing
  through loop iterations until a terminal state is reached.  Each
  epoch (iteration) produces a new state and a new Q-table, to be used
  in the next iteration. Once an episode is over (we reached a
  terminal state), we start a new one from the initial state, but
  using the Q-table of the previous episode.

* IMHO, the best place to start is to implement a function that
  realizes a single iteration of the loop (an epoch) of the learning
  algorithm, making all parameters and return values explicit.  Once
  you settle what is the signature (argument types and return type) of
  this function, it is worth to consult the teachers for a sanity
  check.

* Then add a function that iterates this until a terminal state is
  reached (a training episode), and then another function that
  iterates this a fixed number of times (N times)

* Finally, the policy can be extracted from the Q-table, by producing
  a simpler map: `type Policy[S, A] = Map[S, A]`, which contains the
  highest value action for each state.

* Note that the output of each iteration is randomized, because our
  ε-greedy policy (action selection strategy) picks a random action
  with probability ε.  Use `Rand` from the course to implement
  randomization.  Either use your own Exercise file or obtain a clean
  version of `State.scala` from https://github.com/fpinscala/fpinscala/

## Testing your implementation of Q-Learning

Testing RL is difficult (if you are interested, we publish research in
this area and are happy to supervise MSc projects on the topic).  But
there are some simple tests you can do, if you modularize your
implementation to rule out basic bugs:

* The selection of best action (as part of the ε-greedy selection)
  always returns the highest value action for the state

* The selection of the highest reward (max in the equation) always
  returns the highest reward for that state (all others are smaller)

* The mechanism for tossing ε-biased coined correctly approximates a
  Bernoulli distribution with parameter ε (you can just toss the coin
  10000 times with your function, and check whether the proportion is
  close to ε; for this test an ε == 0.5 might be good, definitely not
  a very small or a very large value).

* You will also need to pick one action uniformly randomly, so it is
  good to test how uniform the function doing this is.

## Implementing the Cliff Walking

Assorted notes:

* Undiscounted means that γ is one (1.0).

* The state of the agent is a pair of numbers from { 0 .. 3 } x { 0 ..
  11 }, with (0,0) being initial, all other states in the bottom row
  being terminal.

* The actions seem to be based modeled as enum: Up, Down, Left, Right

* The agent should also have a function to calculate the initial Q
  table (as this is problem specific) and an initial state

* The key part is the model of the dynamics that says how given a
  state and an action, the agent proceeds to a new state and what
  reward they get in the target state.  Our Cliff Walking problem is
  deterministic, so no need to use Rand in this function.

* It is useful to add a policy printing function to diagnose learning.
  Here is an example how AW printed policies:

```
α=0.1 ε=0.1 γ=1.0 N=400
↓↓↑→↓↑→→↓→→↓
→→↓↓→→→→↓↓↓↓
→→→→→→→→→→→↓
↑...........
```

## Testing Cliff Walking

Once you have the problem implemented more interesting testing is
possible. Here are some ideas

* All rewards are negative (for all states and actions)

* An episode of more than 4 steps is possible (starting with random
  alpha, epsilon, and seed). This requires making sure that the
  episode records intermediate states, or at least counts them (in the
  algorithm's state).  Useful for debugging.

* Any two consecutive states visited by an episode are adjacent (only
  differ on a single axis, and at most by 1). Requires episodes
  storing the history of visited states in the iteration state.

* The initial state is not terminal (easy)

## Training the Agent (Main)

Implement the main function for the agent to train it, and to print
the resulting policy.

* Scala 3 main functions:
  https://docs.scala-lang.org/scala3/book/methods-main-methods.html

* Make sure that `scala-cli run .` works and produces an output for
  the Cliff Walking agent (the resulting policy, and the parameters
  used to train it; see example above). The format of the presentation
  is not fixed.

* It should suffice to train with α=0.1, ε=0.1, γ=1.0, and N=400.

* Note that the Q-Learning (that we are implementing) learns the
  optimal policy, but not the safe policy as SARSA does for this
  problem. So we expect to obtain the policy depicted as the red line,
  not as the blue line on page 132.

## Grading

* Grading is manual

* We run `scala-cli run .` and read the output

* We run `scala-cli test .` and see whether at least 5 tests in total
  are implemented using scalacheck

* We open the files and check whether you are using the prescribed
  libraries (and no imperative constructs are allowed, except for
  `print` and initialization of `RNG`, if you wish---you can also
  just use a constant).

## Workload and Collaboration

* The project is designed for 30h for one person (we have two weeks
  for it), but you are strongly encouraged to work in 2-3 person
  groups this week.

* The amount of code is not large, but the code to be written is
  non-trivial, so do not underestimate the effort.  Here is an
  overview of the size from AW's prototype (this includes 209 lines of
  code of State.scala from the book's github repository).

```
github.com/AlDanial/cloc v 1.90  T=0.01 s (653.7 files/s, 75643.7 lines/s)
--------------------------------------------------------------------------
Language                files          blank        comment           code
--------------------------------------------------------------------------
Scala                       6            124             75            371
--------------------------------------------------------------------------
```

## Tail Recursion issues (Extra Curriculum)

Reinforcement Learning normally requires a lot of iterations (many
thousands epochs in each of the many thousand episodes), so tail
recursion issues are real, relevant, and serious.  For Cliff Walking it
is not necessary to worry about it, as the optimal policy is found
already with about 400 episodes (and if you struggle there, you can
run less episodes and return a sub-optimal policy).

If you try to run several thousand episodes, you will start observing
stack-overflow. This is because, even though our `State` uses tail
calls carefully, JVM does not allow to eliminate tail calls in
non-recursive functions.  (Yes, stack size is a problem even our
functions are not directly recursive.). In the same directory you will
find a more strictly tail recursive version of State (see file
`StateTR.scala`). It is using the Tail Calls machinery (See
https://www.scala-lang.org/api/3.3.0/scala/util/control/TailCalls$.html),
a technique known as trampolining.  The linked page has a link to a
very good article explaining the problem and the solution.  This is
also discussed in Chapter 13 of our book.

If you switch from `State` to `StateTR` you should be able to run
million of episodes.  Trampolining is a very useful thing to know when
you are a functional programmer.

## Hand-in

* Hand in a __zip file containing all .scala files of your project__
  (and nothing more).  Most likely you have one file for Q-Learning,
  one for the cliff-walking example, one for Q-Learning tests, and one
  for cliff-walking tests. You can however choose to organize this
  differently. All the files should be in the root directory of the
  .zip file.

* All files should be in the root directory of the zip, so that if we
  unpack it to an empty directory, we can just run scala-cli on it as
  described above.

* It is allowed to hand-in joint work this week. Write the names of
  all authors on top of the file.

* Each person has to hand in a copy to get a point (but if you do not
  need a point registered, because you already qualified, please only
  hand in one for the group, to save time of the grading persons)
