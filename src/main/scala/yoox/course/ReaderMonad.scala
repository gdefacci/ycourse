package yoox.course

import org.scalacheck.Arbitrary

case class ReaderMonad[C, A](run: C => A) {

  def map[B](f: A => B) = new ReaderMonad[C, B](run.andThen(f))

  def flatMap[B](f: A => ReaderMonad[C, B]) =
    new ReaderMonad[C, B]({ c =>
      f(run(c)).run(c)
    })

}

object ReaderMonad {
  def const[C,A](a:A) = ReaderMonad[C,A](c => a)
  
  def nop[C] = ReaderMonad[C,Unit](c => ())
  
  implicit def arbitraryReaderMonad[C, A](implicit arbA: Arbitrary[A]) = {
    Arbitrary(for {
      a <- arbA.arbitrary
    } yield (new ReaderMonad[C, A]((s1: C) => a)))
  }

  implicit def eqReaderMonad[C, A](implicit eqA: Equal[A], arbS: Arbitrary[C]) = {
    val funEq = Equal.function1[C, A](arbS)
    Equal[ReaderMonad[C, A]]((s1, s2) => funEq(s1.run, s2.run))
  }

}