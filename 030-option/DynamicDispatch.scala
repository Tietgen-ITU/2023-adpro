// Advanced Programming, A. Wąsowski, IT University of Copenhagen

// An example comparing methods (f) and functions (g) in Scala,
// with respect to dynamic and static call dispatch

package adpro.option

class A:
  def f() = println("A")
  val g = () => println("A")

class B extends A:
  override def f() = println("B")
  // B.f overloads A.f
  override val g = () => println("B") 
  // B.g does not overload A.g, but hides it


@main def dynamicDispatchExample = 

  val a1 = A()
  val b1 = B()
  val a2: A = b1 // upcast b1 to A

  a1.f() // prints A, static dispatch
  b1.f() // prints B, static dispatch
  a2.f() // prints B, dynamic dispatch, virtual call, 
         // OO-polymorphism

  println()

  a1.g() // prints A, static dispatch
  b1.g() // prints B, static dispatch
  a1.g() // prints A, static dispatch
