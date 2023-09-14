// Advanced Programming, A. Wąsowski, IT University of Copenhagen
// Based on Functional Programming in Scala, 2nd Edition

package adpro
package variance

// Use sbt command: testOnly adpro.variance.VarianceSpec to see the exception
//
// The test is written in Scala for convenience, but we are testing a problem in
// Java, and running a Java function

object VarianceSpec 
  extends org.scalacheck.Properties("variance"):

  // The test is ignored to avoid confusion, as the expected result is a
  // failure. If you want to run it, uncomment the 'problem' line. 

  property("Variance: Runtime crash caused by covariance of arrays") =
    // Intentionally produces java.lang.ArrayStoreException
    // (to demonstrate the problem with covariance of arrays in Java)
    // Uncomment the line below to run:
    // adpro.variance.Variance.problem()
    true
