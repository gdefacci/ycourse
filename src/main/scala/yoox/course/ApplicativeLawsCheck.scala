package yoox.course

import org.scalacheck.Properties
import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import scala.language.higherKinds
import scala.util.Try
import org.scalacheck.Gen
import scala.util.Success
import scala.util.Failure

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
  
  def successGen[T](implicit gen:Arbitrary[T]):Gen[Try[T]] = gen.arbitrary.map( t => Success(t))
  def failureGen[T](implicit gen:Arbitrary[T]):Gen[Try[T]] = gen.arbitrary.map( t => Failure(new java.lang.Exception("")))
  implicit def tryGen[T](implicit gen:Arbitrary[T]):Arbitrary[Try[T]] = Arbitrary( Gen.oneOf(successGen[T], failureGen[T]) )
  
	new FunctorLawsCheck[Option, Int, String, Boolean](Applicative.option).check
	new ApplicativeLawsCheck[Option, Int, String](Applicative.option).check
  new FunctorLawsCheck[List, Int, String, Boolean](Applicative.list).check
  new ApplicativeLawsCheck[List, Int, String](Applicative.list).check
  new FunctorLawsCheck[HigherKind.EitherR, Int, String, Boolean](Applicative.eitherR).check
  new ApplicativeLawsCheck[HigherKind.EitherR, Int, String](Applicative.eitherR).check

  new FunctorLawsCheck[Try, Int, String, Boolean](Applicative.scalaTry).check
  new ApplicativeLawsCheck[Try, Int, String](Applicative.scalaTry).check

  {
	  implicit val l = Monoid.list[Int]
	  val app:Applicative[HigherKind.Const[List[Int]]#Type] = Applicative.monoid[List[Int]]
    new FunctorLawsCheck[HigherKind.Const[List[Int]]#Type, Long, String, Boolean]( Applicative.monoid[List[Int]] ).check
    new ApplicativeLawsCheck[HigherKind.Const[List[Int]]#Type, Long, String]( Applicative.monoid[List[Int]] ).check
    
  }
  
}