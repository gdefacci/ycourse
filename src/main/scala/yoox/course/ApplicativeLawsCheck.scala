package yoox.course

import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._

import scala.language.higherKinds

class ApplicativeLawsCheck[F[_],A,B](app:Applicative[F])(
    implicit arbFA:Arbitrary[F[A]],
    arbA:Arbitrary[A],
    arbAB:Arbitrary[A => B],
    arbFAB:Arbitrary[F[A => B]],
    eqFA:Equal[F[A]],
    eqFB:Equal[F[B]]) extends Properties("Applicative") {
  
  val laws = new Applicative.Laws(app)
  
  property("identity") = forAll { (a:F[A]) =>
    laws.identity(a)
  }
  
  property("homomorphism") = forAll { (ab:A => B, x:A) =>
    laws.homomorphism(ab, x)
  }
  
  property("interchange") = forAll { (ab:F[A => B], x:A) =>
    laws.interchange(ab, x)
  }
  
}

object ApplicativeLawsCheckMain extends App {
  
	new FunctorLawsCheck[Option, Int, String, Boolean](Applicative.option).check
  new FunctorLawsCheck[List, Int, String, Boolean](Applicative.list).check
  new FunctorLawsCheck[HigherKind.EitherR, Int, String, Boolean](Applicative.eitherR).check
  new FunctorLawsCheck[HigherKind.Endomorphism, Int, String, Boolean](Applicative.endoMorphism).check
  //new FunctorLawsCheck[List, Int, String, Boolean](Applicative.listNonApplicative).check
 
  {
	  implicit val l = Monoid.list[Int]
	  val app:Applicative[({ type Type[T] = List[Int] })#Type] = Applicative.monoid[List[Int]]
    new FunctorLawsCheck[({ type Type[T] = List[Int] })#Type, Long, String, Boolean]( Applicative.monoid[List[Int]] )
    
    println( app.map2[Int,String,List[Int]](List(1,2,3), List(1))( ((a:Int, b:String) => Nil )))
  }
  
}