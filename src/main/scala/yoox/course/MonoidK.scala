package yoox.course

import scala.language.higherKinds

trait MonoidK[F[_]] { self =>
  
  def combine[A](a:F[A], b:F[A]):F[A]
  def empty[A]:F[A]
  
}

object MonoidK {
  
  trait ListMonodiK extends MonoidK[List] {
    def combine[A](a:List[A], b:List[A]):List[A] = a ++ b
    def empty[A]:List[A] = Nil
  }
  
  trait OptionMonodiK extends MonoidK[Option] {
    def combine[A](a:Option[A], b:Option[A]):Option[A] = a.orElse(b)
    def empty[A]:Option[A] = None
  }
  
  implicit lazy val list = new ListMonodiK {}
  implicit lazy val option = new OptionMonodiK {}

}