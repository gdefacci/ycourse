package yoox.course

import scala.language.higherKinds

case class Kleisli[F[_],A,B](val run:A => F[B])(implicit val monad:Monad[F]) {
  
  def andThen[C](k: Kleisli[F, B, C]): Kleisli[F, A, C] =  Kleisli((a: A) => monad.flatMap(run(a))(k.run))  
    
  def map[C](f: B => C): Kleisli[F, A, C] =
    Kleisli( a => monad.map(run(a))(f) )
    
  def flatMap[C](f: B => Kleisli[F, A, C]): Kleisli[F, A, C] =
    Kleisli((r: A) => monad.flatMap[B, C](run(r))(((b: B) => f(b).run(r))))
  
}

object Kleisli {
  
  
  type Reader[E, A] = Kleisli[HigherKind.Id, E, A]
  
  object Reader {
    def apply[E, A](f: E => A): Reader[E, A] = new Kleisli[HigherKind.Id, E, A](f)
  }
  
}
