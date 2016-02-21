package yoox.course

import org.scalacheck.Arbitrary
import org.scalacheck.Prop._
import org.scalacheck.Gen.Parameters
import scala.util.Try
import scala.util.Success
import scala.util.Failure

trait Equal[T] {
  
  def apply(a:T, b:T):Boolean 
  
}

object Equal {
  
  def apply[T](f:(T,T) => Boolean) = new Equal[T] {
    def apply(a:T, b:T):Boolean = f(a,b)
  }
  
  def scalaEq[T] = new Equal[T] {
    def apply(a:T, b:T):Boolean = a == b
  } 
  
  implicit val int = scalaEq[Int]
  implicit val boolean = scalaEq[Boolean]
  implicit val string = scalaEq[String]
  
  implicit def list[T](implicit eq:Equal[T]) = 
    Equal[List[T]]( (l1,l2) => l1.length == l2.length && l1.zip(l2).forall { case (a,b) => eq(a,b) } )
  
  implicit def option[T](implicit eq:Equal[T]) = 
    Equal[Option[T]]( (l1,l2) => if (l1.isEmpty == l2.isEmpty)  l1.zip(l2).forall { case (a,b) => eq(a,b) } else false)
  
  implicit def scalaTry[T](implicit eq:Equal[T]) = 
    Equal[Try[T]]( (l1,l2) => (l1 -> l2) match {
      case (Success(a), Success(b)) => a == b 
      case (Failure(_), Failure(_)) => true 
      case _ => false 
    })
    
  implicit def eitherR[T](implicit eq:Equal[T]) =
    Equal[Either[Throwable, T]]( (l1,l2) => (l1, l2) match {
      case (Left(_), Left(_)) => true 
      case (Right(v1), Right(v2)) => eq(v1,v2)
      case _ => false
    } )
    
  implicit def function1[A,B](implicit arbA:Arbitrary[A]):Equal[A => B] = Equal[A => B] { (f1,f2) =>
    val p = forAll { (a:A) =>
      f1(a) == f2(a)
    }
    val r = p.apply(Parameters.default)
    if (!r.success) p.check
    r.success
  }
  
    
}