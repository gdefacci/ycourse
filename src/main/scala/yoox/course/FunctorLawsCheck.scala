package yoox.course

import scala.language.higherKinds
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Properties

class FunctorLawsCheck[F[_],A,B,C](f:Functor[F])(implicit arbInput:Arbitrary[F[A]], 
    arbF1:Arbitrary[A => B], 
    arbF2:Arbitrary[B => C],
    eqa:Equal[F[A]],
    eqc:Equal[F[C]]) extends Properties("Functor Laws") {
  
  val laws = new Functor.Laws[F](f)
  
  property("composition") = forAll( (fa:F[A], f1:A => B, f2:B => C) => laws.composition(fa, f1, f2) )
  
  property("identity") = forAll( (fa:F[A]) => laws.identity(fa) )
  
}

object FunctorLawsCheckMain extends App {
  
  
  new FunctorLawsCheck[Option,Int,String,Boolean](Functor.option).check
  new FunctorLawsCheck[List,Int,String,Boolean](Functor.list).check
  
}



