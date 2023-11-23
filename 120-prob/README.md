In This Directory
-----------------

The file Exercises.scala contains the exercises for this week. There
is no PDF file with Questions.

The directory contains a prototype implementation of Pigaro, a simple
framework for Bayesian inference, specifically implemented for this
course.  Follow `Basic.sc` for a simple tutorial.

Scala doc has been exposed at:
[[https://www.itu.dk/people/wasowski/ula/scala-doc/index.html]]. The
relevant types to look at are `Pigaro`, `Dist`, and `IData`.

The framework is pure (except for executing `sample`). It is also very
incomplete, but entirely sufficient for solving the proposed
exercises.

The framework is very new. If you miss a function in the API please
request it from Andrzej.



Thesis Topics
-------------

Implementing a complete pure probabilistic programming library in
Scala by extending Pigaro is an interesting research project. This
could go into several directions, possible as MSc theses.  I list the
ideas below. The idea is to release this framework as an open source
project, and let you contribute to this project during a thesis.

## Design of the API and the Internal Domain Specific Language

For the following ideas, the course on _Modeling Language and Systems_
might be useful (but also check my book, the chapter on Internal
DSLs).

- Implement cats and scalaz type class instances for the probabilistic
  programming library so that it becomes compatible with the rest of the
  scala's functional programming ecosystem.

- Investigate using the shapeless library to avoiding nesting within
  tuples (models).  We have that now but using extremely many overloaded
  extensions which is a pain. Perhaps shapeless could reduce this,
  while increasing flexibility for users.

- Support using variable names in queries. For now we only support
  their position in the model (for instance for conditioning).

- Support serialization of samples to Python inference data so that we
  can use matplotlib, arviz, and other excellent plotting libraries in
  the Python universe to present the results of the analysis.

- Implement visualization of hierarchical models using graphviz (PyMC
  can be used as inspiration)

- Implement code generation to PyMC (or another probabilistic
  programming language), to get inference capabilities of that
  framework.

- Support panda-style tabular printing of data (using PP, or using a
  panda-like library for Scala)

- So far `for-yield` does not really do what we need with "if", thus I
  have disabled the support for `if`. It does not condition the entire
  model, but just the last line. So we need a more aggressive syntax
  extension than withFilter. Presumably the macro system can help.

- Make a Turing-style syntax (Julia) using macros in Scala 3.


## Extension of the Probabilistic Inference Capabilities

For working on the following ideas, the course on _Probabilistic
Programming_ might be useful.

- Implement other sampling methods, especially Metropolis-Hastings,
  but also importance sampling, Gibbs and Hamiltonian Monte-Carlo.
  This will enable support for continuous variables as well.

- Implement factory methods for continuous distributions (And more
  discrete distributions).

- Support generating multiple chains within `IData` and computing
  convergence diagnosis statistics.

- Try implementing variable elimination (symbolic inference). This
  might be difficult with a shallowly embedded internal DSL, but perhaps
  an alternative deep embedding representation can be created without
  changing the API. This aspect crosses into to DSL design realm of work
  as well.

- Support vectorized variables

- Support observations of many data points for continuous variables.
  Implement scoring and then support likelihoods for simple outcomes and
  for vectorized outcomes

- Support automatic sampler selection.

- Implement exercise sets from the puppies book (Kruschke), and from
  the Statistical Rethinking book (McElreath) from Statistical
  Rethinking book. These will be useful for educational purposes
  (teaching probabilistic programming).1

- Switch the framework to work with log-probabilisties

