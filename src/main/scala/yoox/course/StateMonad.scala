package yoox.course

import org.scalacheck.Arbitrary

class StateMonad[S, A](val run: S => (A, S)) {

  def map[B](f: A => B) = new StateMonad[S, B]({ s =>
    val (a, s1) = run(s)
    f(a) -> s1
  })

  def flatMap[B](f: A => StateMonad[S, B]) = new StateMonad[S, B]({ s =>
    val (a, s1) = run(s)
    f(a).run(s)
  })

}

object StateMonad {
  implicit def arbitraryStateMonad[S, A](implicit arbA: Arbitrary[A]) = {
    Arbitrary(for {
      a <- arbA.arbitrary
    } yield (new StateMonad[S, A]((s1: S) => (a, s1))))
  }

  implicit def eqStateMonad[S, A](implicit eqA: Equal[A], arbS: Arbitrary[S]) = {
    val funEq = Equal.function1[S, (A, S)](arbS)
    Equal[StateMonad[S, A]]((s1, s2) => funEq(s1.run, s2.run))
  }

}