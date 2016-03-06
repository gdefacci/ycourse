package yoox.course

import scala.language.higherKinds 

trait Monad[F[_]] extends Applicative[F] {
  
  def pure[A](a:A):F[A]
  
  def flatMap[A,B](fa:F[A])(f:A => F[B]):F[B] 

  override def map[A,B](fa:F[A])(f:A => B):F[B] = 
    flatMap(fa)(f.andThen(pure))
  
  def apply[A,B](fa:F[A])(f:F[A => B]):F[B] = 
    flatMap(f)( f => flatMap(fa)(f.andThen(pure)) )
  
}

object Monad {
  
  implicit lazy val option = new Monad[Option] {
    def pure[A](a:A) = Some(a)
    def flatMap[A,B](fa:Option[A])(f:A => Option[B]):Option[B] = fa.flatMap(f) 
  }

  implicit lazy val list = new Monad[List] {
    def pure[A](a:A) = List(a)
    def flatMap[A,B](fa:List[A])(f:A => List[B]):List[B] = fa.flatMap(f) 
  }

  implicit def state[S] = new Monad[({ type Type[T] = StateMonad[S,T] })#Type] {
    def pure[A](a:A) = new StateMonad[S,A]( s => a -> s)
    def flatMap[A,B](fa:StateMonad[S,A])(f:A => StateMonad[S,B]):StateMonad[S, B] = fa.flatMap(f)
  }
  
  implicit def reader[C] = new Monad[({ type Type[T] = ReaderMonad[C,T] })#Type] {
    def pure[A](a:A) = new ReaderMonad[C,A]( s => a )
    def flatMap[A,B](fa:ReaderMonad[C,A])(f:A => ReaderMonad[C,B]):ReaderMonad[C, B] = fa.flatMap(f)
  }

  implicit def identityMonad = new Monad[Identity] {
    def pure[A](a:A) = new Identity[A](a)
    def flatMap[A,B](fa:Identity[A])(f:A => Identity[B]):Identity[B] = f(fa.value)
  }
}
