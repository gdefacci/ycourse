package yoox.course

import scala.language.higherKinds 

/**
 * Applicative programming with effects
 * 
 * http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf
 */
trait Applicative[F[_]] extends Functor[F] { self => 
  
  def pure[A](a:A):F[A]
  
  def apply[A,B](v:F[A])(f:F[A => B]):F[B]
  
  def apply2[A,B,C](a:F[A], b:F[B])(f:F[(A,B) => C]):F[C] = 
    apply(a)(apply(b)( map(f)( (f:(A,B) => C) => b => a => f(a,b)) ))
  
  def apply3[A,B,C,D](a:F[A], b:F[B], c:F[C])(f:F[(A,B,C) => D]):F[D] = 
    apply(a)(apply(b)(apply(c)( map(f)( (f:(A,B,C) => D) => c => b => a => f(a,b,c)) )))
  
  def tuple2[A,B](a:F[A], b:F[B]):F[(A,B)] = 
    map2(a,b)((a,b) => a -> b)

  def map[A,B](v:F[A])(f:A => B):F[B] = apply(v)(pure(f))
  
  def map2[A,B,C](a:F[A], b:F[B])(f1:(A, B) => C):F[C] =
    apply(a)(map(b)((b:B) => (a:A) => f1(a,b) ))
  
  def map3[A,B,C,D](a:F[A], b:F[B], c:F[C])(f1:(A, B, C) => D):F[D] =
    apply(a)(apply(b)(map(c)( c => b => a => f1(a,b,c) )))
  
  def flip[A,B](f:F[A => B]):F[A] => F[B] = fa =>
    apply(fa)(f)
  
  def compose[G[_]](app1:Applicative[G]):Applicative[({ type Type[T] = F[G[T]] })#Type] = 
    new Applicative[({ type Type[T] = F[G[T]] })#Type] {
        def pure[A](a:A):F[G[A]] = self.pure(app1.pure(a))
  
        def apply[A,B](fg:F[G[A]])(f:F[G[A => B]]):F[G[B]] = {
          val x : F[G[A] => G[B]] = self.map(f)(app1.flip)
          self.apply(fg)( x )
        }
        
    }
}

object Applicative {
  
  val option:Applicative[Option] = new Applicative[Option] {
    def pure[A](a:A) = Some(a)
    def apply[A,B](v:Option[A])(f:Option[A => B]):Option[B] =
      v.zip(f).headOption.map( p => p._2(p._1))
      
  }
  
  val list = new Applicative[List] {
    def pure[A](a:A) = List(a)
    def apply[A,B](v:List[A])(f:List[A => B]):List[B] =
     v.flatMap( value => f.map(f => f(value)))
  }
  
  val listReverse = new Applicative[List] {
    def pure[A](a:A) = List(a)
    def apply[A,B](v:List[A])(f:List[A => B]):List[B] =
     v.reverse.flatMap( value => f.reverse.map(f => f(value)))
  }
  
  val listNonApplicative = new Applicative[List] {
    def pure[A](a:A) = List(a)
    def apply[A,B](v:List[A])(f:List[A => B]):List[B] =
     v.zip(f).map( p => p._2(p._1) )
  }
  
  import HigherKind._
  
  val eitherR = new Applicative[EitherR] {
    def pure[A](a:A) = Right(a)
    def apply[A,B](v:EitherR[A])(f:EitherR[A => B]):EitherR[B] = (v,f) match {
      case (Left(err), _) => Left(err)
      case (_, Left(err)) => Left(err)
      case (Right(v), Right(f)) => Right(f(v))
    }
  }
  
  val endoMorphism = new Applicative[Endomorphism] {
    def pure[A](a:A) = (a1:A) => a
    def apply[A,B](v:A => A)(f:(A => B) => (A => B)):B  => B = { b => 
      b
    }
    
  }
  
  def monoid[M](implicit m:Monoid[M]):Applicative[({ type Type[T] = M  })#Type] = new Applicative[({ type Type[T] = M })#Type] {
   def apply[A, B](v: M)(f: M): M = m.append(v, f)
   def pure[A](a: A): M = m.zero 
  }
  
  
  /**
   * https://en.wikibooks.org/wiki/Haskell/Applicative_functors#Applicative_functor_laws
   * 
   * pure id <*> v = v                            -- Identity
   * pure f <*> pure x = pure (f x)               -- Homomorphism
   * u <*> pure y = pure ($ y) <*> u              -- Interchange
   * pure (.) <*> u <*> v <*> w = u <*> (v <*> w) -- Composition
   *
   * Legenda
   * 
   * <*> 	: apply, the parameters are in the opposite order
   * (.) 	: andThen
   * id 	: identity function
   *  
   */
  class Laws[F[_]](app:Applicative[F]) {
    
    def identity[T](v:F[T])(implicit eq:Equal[F[T]]) =
      eq( app.apply(v)(app.pure( (i:T) => i)), v)
    
    def homomorphism[A,B](f:A => B, x:A)(implicit eq:Equal[F[B]]) = 
      eq(app.apply( app.pure(x) )( app.pure(f) ),  app.pure(f(x)) )
    
    def interchange[A,B](u:F[A => B], y:A)(implicit eq:Equal[F[B]]) = {
      eq( app.apply(app.pure(y))(u),
          app.apply(u)(app.pure( (f:A => B) => f(y))) )
    }
      
  }
  
  
}