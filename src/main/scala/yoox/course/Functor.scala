package yoox.course

import scala.language.higherKinds 

trait Functor[F[_]] { self =>

  def map[A,B](v:F[A])(f:A => B):F[B] 
  
  def compose[G[_]](fb:Functor[G]) = new Functor[({ type Type[T] = F[G[T]] })#Type] {
    def map[A,B](v:F[G[A]])(f:A => B):F[G[B]] = {
      self.map(v:F[G[A]])( (v1:G[A]) => fb.map(v1)(f) )
    }
  }
  
  def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  
  def product[G[_]](fb:Functor[G]) = new Functor[({ type Type[T] = (F[T], G[T]) })#Type] {
    def map[A,B](v:(F[A], G[A]))(f:A => B):(F[B], G[B]) = {
      self.map(v._1)(f) -> fb.map(v._2)(f)
    } 
  }
  
}

object Functor {
  
  type Parser[T] = String => Option[(T,String)]
  
  implicit val list = new Functor[List] {
    def map[A,B](v:List[A])(f:A => B):List[B] = v.map(f)
  }
  
  implicit val option = new Functor[Option] {
    def map[A,B](v:Option[A])(f:A => B):Option[B] = v.map(f)
  }
  
  implicit val parser = new Functor[Parser] {
    def map[A,B](v:Parser[A])(f:A => B):Parser[B] = 
      v.andThen( opt => opt.map( p => f(p._1) -> p._2 ))
  }
  
  val s1 = parser.compose(option)

  class Laws[F[_]](f:Functor[F]) {
    
    def composition[A,B,C](a:F[A], f1:A => B, f2:B => C)(implicit eq:Equal[F[C]]):Boolean = 
     eq(f.map( f.map(a)(f1) )(f2), f.map(a)(f1 andThen f2)) 
    

    def identity[A](a:F[A])(implicit eq:Equal[F[A]]):Boolean = 
     eq(f.map(a)(id => id), a) 
    
  }
  
}
