package yoox.course

import org.scalacheck.Prop._
import org.scalacheck.Arbitrary._
import scala.language.higherKinds
import org.scalacheck.Arbitrary
import org.scalacheck.Properties

class MonadLawsCheck[F[_],A,B,C](desc:String)(implicit m:Monad[F], 
    arbA:Arbitrary[A], 
    arbFA:Arbitrary[F[A]], 
    arbF1:Arbitrary[A => F[B]], 
    arbF2:Arbitrary[B => F[C]],
    eqFA:Equal[F[A]],
    eqFC:Equal[F[C]]) extends Properties(s"Monad $desc") {

  lazy val laws = new MonadLaws()
  
  property("flatMapAssociativity") = forAll { (fa:F[A], f1:A => F[B], f2:B => F[C]) =>
    laws.flatMapAssociativity(fa, f1, f2)
  }
  
  property("leftIdentity") = forAll { (a:A) =>
    laws.leftIdentity(a)
  }
  
  property("rightIdentity") = forAll { (a:F[A]) =>
    laws.rightIdentity(a)
  }
  
}

object MonadLawsCheckMain extends App {
  
  new MonadLawsCheck[Option,Int,String,Boolean]("Option").check
  new MonadLawsCheck[List,Int,String,Boolean]("List").check
  new MonadLawsCheck[HigherKind.State[Int]#Type,Int,String,Boolean]("State").check
  new MonadLawsCheck[HigherKind.Reader[Int]#Type,Int,String,Boolean]("Reader").check
  new MonadLawsCheck[Identity,Int,String,Boolean]("Identity").check
  
}
