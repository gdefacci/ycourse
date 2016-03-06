package yoox.course

import scala.language.higherKinds 

class MonadLaws[F[_]](implicit val monad:Monad[F]) {

  import monad._
  
  def flatMapAssociativity[A,B,C](fa:F[A], f1:A => F[B], f2:B => F[C])(implicit eq:Equal[F[C]]) = 
    eq( 
        flatMap(flatMap(fa)(f1))(f2), 
        flatMap(fa)( v => flatMap(f1(v))(f2) )
        )
  
  
  def rightIdentity[A](fa:F[A])(implicit eq:Equal[F[A]]) = 
    eq(  flatMap( fa )( i => pure(i) ), 
         fa )     
  
  def leftIdentity[A](a:A)(implicit eq:Equal[F[A]]) = 
    eq(  flatMap( pure(a) )( i => pure(i) ), 
         pure(a) )     
  
  
  
}