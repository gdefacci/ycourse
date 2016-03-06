package yoox.course

import org.scalacheck.Arbitrary

class Identity[A](val value:A) {
  
  def map[B](f:A => B) = new Identity(f(value))
  
  def flatMap[B](f:A => Identity[B]):Identity[B] = f(value)
  
}

object Identity {
  
  implicit def arbitraryIdentityMonad[A](implicit arbA: Arbitrary[A]) = 
    Arbitrary(for {
      a <- arbA.arbitrary
    } yield (new Identity[A](a)))

    
  implicit def eqIdentityMonad[A](implicit eqA: Equal[A]) = {
    Equal[Identity[A]]((s1, s2) => eqA(s1.value, s2.value))
  }

  
}
