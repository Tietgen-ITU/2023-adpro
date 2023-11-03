// Advanced Programming, A. Wąsowski, IT University of Copenhagen 
// A State variant that rewrites everything into CPS with
// trampolining. Based on https://github.com/runarorama/Days2012

package adpro.stateTR

import scala.util.control.TailCalls.*

opaque type StateTR[S, +A] = S => TailRec[(A, S)]

object StateTR:

  extension [S, A](underlying: StateTR[S, A])
    def run(s: S): TailRec[(A, S)] = underlying(s)

    def map[B](f: A => B): StateTR[S, B] =
      s => run(s).map { (a, s1) => (f(a), s1) }

    def flatMap[B](f: A => StateTR[S,B]): StateTR[S, B] =
      s => 
        tailcall(run(s).flatMap { (a, s1) => tailcall(f(a).run(s1)) })

    def map2[B, C](sb: StateTR[S, B])(f: (A, B) => C): StateTR[S, C] =
      underlying.flatMap { a => sb.map { b => f(a, b) } }

    def until(p: S => Boolean)(initial: S): TailRec[S] = 
      if p(initial) then done(initial)
      else underlying(initial)
        .map { _._2 }
        .flatMap(until(p))

  def apply[S, A](f: S => TailRec[(A, S)]): StateTR[S, A] = f

  def unit[S, A](a: => A): StateTR[S, A] = s => tailcall(done(a, s))

  def tr[A, B](f: A => B): A => TailRec[B] =
    a => tailcall(done(f(a)))

  def fLift[A, B](f: A => StateTR[A,A]): StateTR[A, A] => StateTR[A, A] = 
    sab => sab.flatMap(f)

  def sequence[S, A](actions: LazyList[StateTR[S, A]])
    : StateTR[S,  LazyList[A]] =
    val z = unit[S, LazyList[A]](LazyList.empty)
    StateTR(actions.foldRight(z)((s, acc) => s.map2(acc)(_ #:: _)))
