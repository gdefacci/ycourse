package yoox.course.samples

import scala.util.Try
import scala.util.Success
import scala.util.Failure
import scala.Left
import scala.Right

import scala.language.higherKinds

trait Validator[R[_]] {
  
  def success[T](v:T):R[T]
  def error[T](err:Exception):R[T]
  
}

case class IntError(n:Int) extends Exception(s"$n")

object GreaterThanThree {
  
  def apply[R[_]](n:Int)(implicit vf:Validator[R]):R[Int] = {
    if (n > 3) vf.success(n)
    else vf.error(new IntError(n))
  }
  
}

object Validator {
  
  implicit val option = new Validator[Option] {
    def success[T](v:T) = Some(v)
    def error[T](err:Exception) = None
  }
  
  implicit val scalaTry = new Validator[Try] {
    def success[T](v:T) = Success(v)
    def error[T](err:Exception) = Failure(err)
  }
  
  type EitherR[T] = Either[Exception,T]
  
  implicit val eitherErrorOr = new Validator[EitherR] {
    def success[T](v:T):Either[Exception, T] = Right(v)
    def error[T](err:Exception):Either[Exception, T] = Left(err)
  }
  
}

object MyValidatorMain extends App {
  
  assert(None == GreaterThanThree[Option](3))
  assert(Some(5) == GreaterThanThree[Option](5))
  
  assert(Failure(IntError(3)) == GreaterThanThree[Try](3))
  assert(Success(5) == GreaterThanThree[Try](5))
  
  assert(Left(IntError(3)) == GreaterThanThree[Validator.EitherR](3))
  assert(Right(5) == GreaterThanThree[Validator.EitherR](5))
  
}